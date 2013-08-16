{

docxvectorialwriter.pas

THIS IS A UNIT CURRENTLY UNDER DEVELOPEMENT

      Current Functionality

          - Text support
          - Paragraph Style Support
          - Character Style Support

      TODO
          - Complete support for current FPVectorial concepts
            - Support multiple PageSequences (TODO Bookmarked in code)
            - Add Dimension support to each PageSequence
            - Implement Header and Footer (TODO Bookmarked in code)

          - Add following to both FPVectorial AND DOCXWriter
            - Add Table Support
            - Add Image Support (From file and from Stream, in Paragraph and in Table)
            - Add simple Work Fields (NumPage, PageCount, Filename (Full, part), DatePrinted)
            - Add Pagebreak support
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

Change History  (To be deleted once added to SVN)
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
}

unit docxvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  zipper, {NOTE: might require zipper from FPC 2.6.2+ }
  fpimage, fpcanvas,
  fpvectorial, fpvutils, lazutf8;

type

  // Here to just to ensure the resulting xml files are pretty :-)

  { TIndentedStringList }

  TIndentedStringList = Class(TStringList)
  Private
    FIndent : String;
    FIndentSteps : String;
  Public
    Constructor Create;
    Function Add(const S: String): Integer; Override;
    Function Add(bIndent : Boolean; const S:String) : Integer;

    Procedure DecIndent;
  end;

  { TvDOCXVectorialWriter }


  TvDOCXVectorialWriter = class(TvCustomVectorialWriter)
  private
    FData : TvVectorialDocument;
    FDocument : TIndentedStringList;
    FStyles : TIndentedStringList;

    Procedure PrepareDocument;
    Function PrepareRootRelationships : String;
    Function PrepareContentTypes : String;
    Procedure PrepareStyles;
    Procedure PrepareTextRunStyle(ADoc : TIndentedStringList; AStyle : TvStyle);
    Function PrepareDocumentRels : String;
    Function VectorialStyleToDOCXStyleID(AStyle : TvStyle) : String;
  public
    { General reading methods }
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFile(AFileName: string; AData: TvVectorialDocument); override;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

// Generic Helper Units
Function PointsToTwipsS(APoints : Double) : String;
Function mmToTwipsS(AMillimetres : Double) : String;

implementation

Const
  { OOXML general XML constants }
  { OOXML Directory structure constants }
  OOXML_PATH_TYPES              = '[Content_Types].xml';
  OOXML_PATH_RELS_RELS          = '_rels/.rels';
  OOXML_PATH_WORD_DOCUMENT_RELS = 'word/_rels/document.xml.rels';
  OOXML_PATH_WORD_DOCUMENT      = 'word/document.xml';
  OOXML_PATH_WORD_STYLES        = 'word/styles.xml';

Function PointsToTwipsS(APoints: Double): String;
begin
  // 1 Twip = 1/20 of a point
  Result := IntToStr(Round(20* APoints));
end;

Function mmToTwipsS(AMillimetres: Double): String;
begin
  // 1 Twip = 1 / 1440 of an inch - sigh...
  Result := IntToStr(Round(0.0393701 * 1440 * AMillimetres));
end;

{ TIndentedStringList }

Constructor TIndentedStringList.Create;
begin
  FIndent := '';
  FIndentSteps := '  ';
end;

Function TIndentedStringList.Add(const S: String): Integer;
begin
  Result:=inherited Add(FIndent + S);
end;

Function TIndentedStringList.Add(bIndent: Boolean; const S: String): Integer;
begin
  If bIndent Then
    FIndent := FIndent + FIndentSteps;

  Result := inherited Add(FIndent + S);

  If not bIndent Then
    DecIndent;
end;

Procedure TIndentedStringList.DecIndent;
begin
  FIndent := Copy(FIndent, 1, Length(FIndent) - Length(FIndentSteps));
end;

{ TvDOCXVectorialWriter }

constructor TvDOCXVectorialWriter.Create;
begin
  inherited Create;

  FDocument := TIndentedStringList.Create;
  FStyles := TIndentedStringList.Create;
end;

destructor TvDOCXVectorialWriter.Destroy;
begin
  FDocument.Free;
  FStyles.Free;

  inherited Destroy;
end;

Function TvDOCXVectorialWriter.PrepareContentTypes: String;
begin
  // first, the minimum requirements for a bare-bones .docx file
  Result := '<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' + LineEnding +
            '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'+ LineEnding +
            '  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'+ LineEnding +
            '  <Default Extension="xml" ContentType="application/xml"/>'+ LineEnding+
            '  <Override PartName="/'+OOXML_PATH_WORD_DOCUMENT+'" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>'+ LineEnding;

  // Determine what other files we need to append
  Result := Result + '  <Override PartName="/'+OOXML_PATH_WORD_STYLES+'" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>' + LineEnding;

  Result := Result + '</Types>';
end;

Function TvDOCXVectorialWriter.PrepareRootRelationships: String;
begin
  Result := '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + LineEnding +
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' + LineEnding +
            '  <Relationship Id="rId21" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="/'+OOXML_PATH_WORD_DOCUMENT+'"/>' + LineEnding +
            '</Relationships>';
end;

Procedure TvDOCXVectorialWriter.PrepareDocument;
Var
  oPage : TvPage;
  oPageSequence : TvTextPageSequence;
  oPageEntity : TvEntity;
  iPage, i : Integer;

  Procedure AddParagraphProperties(AStyle : TvStyle);
  Begin
    FDocument.Add(True, '<w:pPr>');
    FDocument.Add('  <w:pStyle w:val="'+VectorialStyleToDOCXStyleID(AStyle)+'"/>');
    FDocument.Add(False, '</w:pPr>');
  end;

  Procedure AddRunProperties(AStyle : TvStyle);
  Begin
    FDocument.Add(True, '<w:rPr>');
    FDocument.Add('  <w:rStyle w:val="'+VectorialStyleToDOCXStyleID(AStyle)+'"/>');
    FDocument.Add(False, '</w:rPr>');
  end;

  Procedure AddTextRun(sText: String; AStyle : TvStyle);
  begin
    // Strip out the trailing line break
    If sText<>'' Then
    Begin
      If DefaultTextLineBreakStyle=tlbsCRLF Then
        sText := Copy(sText, 1, Length(sText) - 2)
      Else
        sText := Copy(sText, 1, Length(sText) - 1)
    end;

    FDocument.Add(True, '<w:r>');

    If Assigned(AStyle) Then
      AddRunProperties(AStyle);

    FDocument.Add('  <w:t xml:space="preserve">'+sText+'</w:t>');
    FDocument.Add(False, '</w:r>');
  end;

  Procedure ProcessParagraph(AParagraph : TvParagraph);
  Var
    oParagraphEntity : TvEntity;
  Begin
    FDocument.Add(True, '<w:p>');

    If Assigned(AParagraph.Style) Then
      AddParagraphProperties(AParagraph.Style);

    oParagraphEntity := AParagraph.GetFirstEntity;

    While oParagraphEntity<>Nil Do
    Begin
      If oParagraphEntity Is TvText Then
        AddTextRun(TvText(oParagraphEntity).Value.Text, TvText(oParagraphEntity).Style)
      Else
        Raise Exception.Create('Unsupported Entiry: '+oParagraphEntity.ClassName);
      // TODO What other entities in TvParagraph do I need to care for

      oParagraphEntity := AParagraph.GetNextEntity;
    end;

    FDocument.Add(False,'</w:p>');
  End;

begin
  FDocument.Clear;
  FDocument.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
  FDocument.Add('<w:document xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">');

  For iPage := 0 To FData.GetPageCount-1 Do
  Begin
    oPage := FData.GetPageAsText(iPage);

    If oPage Is TvTextPageSequence Then
    Begin
      oPageSequence := TvTextPageSequence(oPage);
      // TODO Do something with oPageSequence.Header and .Footer
      //      (Each pagesequence should get its own headern.xml and footern.xml)

      // TODO Implement Multiple PageSequences.  Currently only works with one PageSequent
      FDocument.Add(True, '<w:body>');

      For i := 0 To oPageSequence.GetEntitiesCount-1 Do
      Begin
        oPageEntity := oPageSequence.GetEntity(i);

        If oPageEntity Is TvParagraph Then
          ProcessParagraph(TvParagraph(oPageEntity))
        Else
          // TODO: What other entities in oPageSequence do I need to care for?
          Raise Exception.Create('Unsupported Entity: '+oPageEntity.ClassName);
      end;

      FDocument.Add(False, '</w:body>');
    end;
  end;

  FDocument.Add('</w:document>');
end;


Function TvDOCXVectorialWriter.VectorialStyleToDOCXStyleID(AStyle : TvStyle) : String;
  // TODO This code could be changed if StyleNames are not allowed to be empty
  Function FindIndex(AStyle : TvStyle) : Integer;
  Var
    i : Integer;
  Begin
    i := 0;

    While (i<FData.GetStyleCount) And (FData.GetStyle(i)<>AStyle) Do
      Inc(i);

    If i<FData.GetStyleCount Then
      Result := i
    Else
      Result := -1;
  End;
Begin
  If Trim(AStyle.name)<>'' Then
    Result := StringReplace(AStyle.Name, ' ', '', [rfReplaceAll, rfIgnoreCase])
  Else
  Begin
    Result := Format('StyleID%d', [FindIndex(AStyle)]);
    AStyle.Name := Result;   // Saves having to do the FindIndex later...
  end;
end;

Procedure TvDOCXVectorialWriter.PrepareTextRunStyle(ADoc: TIndentedStringList;
  AStyle: TvStyle);
begin
  ADoc.Add(True, '<w:rPr>');

  If (spbfFontName In AStyle.SetElements) And (AStyle.Font.Name<>'') Then
    ADoc.Add('  <w:rFonts w:ascii="'+AStyle.Font.Name+'" w:hAnsi="'+AStyle.Font.Name+'"/>');

  if spbfFontSize In AStyle.SetElements Then
    // TODO Where does the magic * 2 come from?  Confirm...
    ADoc.Add('  <w:sz w:val="'+IntToStr(2*AStyle.Font.Size)+'"/>');

  if AStyle.Font.Bold Then
    ADoc.Add('  <w:b w:val="on"/>');

  if AStyle.Font.Italic Then
    ADoc.Add('  <w:i w:val="on"/>');

  if AStyle.Font.Underline Then
    ADoc.Add('  <w:u w:val="on"/>');

  if AStyle.Font.StrikeThrough Then
    ADoc.Add('  <w:strike w:val="on"/>');

  if CompareColors(AStyle.Font.Color, FPColor(0, 0, 0, 0))<>0 Then
    ADoc.Add('  <w:color w:val="'+FPColorToRGBHexString(AStyle.Font.Color)+'"/>');

  // Can Word handled re-oriented text?  I hope not...
  If AStyle.Font.Orientation<>0 Then;

  If ADoc[ADoc.Count-1]<>'<w:rPr>' Then
    ADoc.Add(False, '</w:rPr>')
  Else
  Begin
    ADoc.Delete(ADoc.Count-1);
    ADoc.DecIndent;
  end;
end;

Procedure TvDOCXVectorialWriter.PrepareStyles;
  Procedure PrepareParagraphStyle(AStyle : TvStyle; AType : String = 'paragraph');
  Var
    sTemp : String;
  Begin
    FStyles.Add(True, '<w:style w:type="'+AType+'" w:styleId="'+VectorialStyleToDOCXStyleID(AStyle)+'">');

    // Add the name and inheritance values
    FStyles.Add('  <w:name w:val="'+AStyle.Name+'"/>');

    If Assigned(AStyle.Parent) Then
      FStyles.Add('  <w:basedOn w:val="'+VectorialStyleToDOCXStyleID(AStyle.Parent)+'"/> ');

    // TODO This doesn't always need to be set, but I don't yet understand the rules...
    FStyles.Add('  <w:qFormat/> ');   // Latent Style Primary Style Setting.

    // TODO Spec cries if you redeclare a identical property declared in a parent
    //      At the moment code is relying on Styles correctly defined up through hierarchy

    If AType='paragraph' Then
    Begin
      // Add the Paragraph Properties
      FStyles.Add(True, '<w:pPr>');

      sTemp := '';
      If AStyle.MarginTop<>0 Then
        sTemp := sTemp + ' w:before="'+mmToTwipsS(AStyle.MarginTop)+'"';

      If AStyle.MarginBottom<>0 Then
        sTemp := sTemp + ' w:after="'+mmToTwipsS(AStyle.MarginBottom)+'"';

      If sTemp<>'' Then
        FStyles.Add('  <w:spacing'+ sTemp+'/>');

      sTemp := '';
      If AStyle.MarginLeft<>0 Then
        sTemp := sTemp + ' w:left="'+mmToTwipsS(AStyle.MarginLeft)+'"';

      If AStyle.MarginRight<>0 Then
        sTemp := sTemp + ' w:right="'+mmToTwipsS(AStyle.MarginRight)+'"';

      If sTemp<>'' Then
        FStyles.Add('  <w:ind'+ sTemp+'/>');

      If FStyles[FStyles.Count-1]<>'<w:pPr>' Then
        FStyles.Add(False, '</w:pPr>')
      Else
      Begin
        FStyles.Delete(FStyles.Count-1);
        FStyles.DecIndent;
      end;
    end;

    // Now add the actual formatting (rPr = Run Properties).
    PrepareTextRunStyle(FStyles, AStyle);

    FStyles.Add(False, '</w:style>  ');
  end;
Var
  i : Integer;
  oStyle : TvStyle;
begin
  FStyles.Clear;
  FStyles.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
  FStyles.Add('<w:styles xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">');

  For i := 0 To FData.GetStyleCount-1 Do
  Begin
    oStyle := FData.GetStyle(i);

    // TODO handle the other StyleKinds
    If oStyle.GetKind in [vskTextBody, vskHeading] Then
      PrepareParagraphStyle(oStyle)
    Else if oStyle.GetKind in [vskTextSpan] Then
      PrepareParagraphStyle(oStyle, 'character')
    Else
      Raise Exception.Create('Unsupported StyleKind in '+oStyle.Name);
  end;

  FStyles.Add('</w:styles>');
end;

Function TvDOCXVectorialWriter.PrepareDocumentRels: String;
begin
  Result := '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + LineEnding +
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">';

  Result := Result + '    <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>' + LineEnding;
//            '    <Relationship Id="rId2" Type="http://schemas.microsoft.com/office/2007/relationships/stylesWithEffects" Target="stylesWithEffects.xml"/>' + LineEnding +
//            '    <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" Target="settings.xml"/>' + LineEnding +
//            '    <Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings" Target="webSettings.xml"/>' + LineEnding +
//            '    <Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable" Target="fontTable.xml"/>' + LineEnding +
//            '    <Relationship Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme" Target="theme/theme1.xml"/>' + LineEnding +
  Result := Result + '</Relationships>';
end;

procedure TvDOCXVectorialWriter.WriteToFile(AFileName: string; AData: TvVectorialDocument);
var
  oStream: TFileStream;
begin
  If ExtractFileExt(AFilename)='' Then
    AFilename := AFilename + '.docx';

  oStream:=TFileStream.Create(AFileName,fmCreate);
  try
    WriteToStream(oStream, AData);
  finally
    FreeAndNil(oStream);
  end;
end;

procedure TvDOCXVectorialWriter.WriteToStream(AStream: TStream; AData: TvVectorialDocument);
var
  oContentTypes: TStringStream;
  oRelsRels: TStringStream;
  oDocument : TStringStream;

  oDocumentRels : TStringStream;
  oStyles : TStringStream;

  oZip: TZipper;

begin
  FData := AData;

  PrepareDocument;
  PrepareStyles;

  oContentTypes := TStringStream.Create(PrepareContentTypes);
  oRelsRels := TStringStream.Create(PrepareRootRelationships);

  // TODO Research using a TMemoryStream or similar for the TStringLists...
  oStyles := TStringStream.Create(FStyles.Text);
  oDocumentRels := TStringStream.Create(PrepareDocumentRels);
  oDocument := TStringStream.Create(FDocument.Text);

  oZip := TZipper.Create;
  try
    oZip.Entries.AddFileEntry(oContentTypes, OOXML_PATH_TYPES);
    oZip.Entries.AddFileEntry(oRelsRels, OOXML_PATH_RELS_RELS);
    oZip.Entries.AddFileEntry(oDocument, OOXML_PATH_WORD_DOCUMENT);

    oZip.Entries.AddFileEntry(oStyles, OOXML_PATH_WORD_STYLES);
    oZip.Entries.AddFileEntry(oDocumentRels, OOXML_PATH_WORD_DOCUMENT_RELS);

    oZip.SaveToStream(AStream);
  finally
    oZip.Free;

    oStyles.Free;
    oDocumentRels.Free;

    oContentTypes.Free;
    oRelsRels.Free;
    oDocument.Free;
  end;
end;

initialization

  RegisterVectorialWriter(TvDOCXVectorialWriter, vfDOCX);

end.

