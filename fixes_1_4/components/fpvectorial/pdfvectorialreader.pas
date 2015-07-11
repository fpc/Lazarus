{
pdfvectorialreader.pas

Reads the vectorial information form a PDF file

PDF file format specification obtained from:

ADOBE SYSTEMS INCORPORATED. PDF Reference: Adobe®
Portable Document Format. San Jose, 2006. (Sixth edition).

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit pdfvectorialreader;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  pdfvrlexico, pdfvrsintatico, pdfvrsemantico, avisozlib,
  fpvectorial;

type

  { TvPDFVectorialReader }

  TvPDFVectorialReader = class(TvCustomVectorialReader)
  private
    procedure WriteStringToStream(AStream: TStream; AString: string);
  public
    { public to allow uncompressing PDFs independently }
    function getFirstPage(AInput: TStream; AOutput: TStream):PageHeader;
    procedure unzipPage(AInput: TStream; AOutput: TStream);
    procedure translatePage(AInput: TStream; AData: TvVectorialDocument;
              APageHeader: PageHeader);
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvPDFVectorialReader }

procedure TvPDFVectorialReader.WriteStringToStream(AStream: TStream;
  AString: string);
begin
  AStream.WriteBuffer(AString[1], Length(AString));
end;

function TvPDFVectorialReader.getFirstPage(AInput: TStream; AOutput: TStream): PageHeader;
var
  mytoken: Token;
  myAnLexicoPage: AnLexico;
  myAnLexicoContents: AnLexico;
  myAnSintaticoPage: AnSintaticoPage;
  myAnSintaticoContents: AnSintaticoPageContents;
  AInput2: TStream;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':> TvPDFVectorialReader.getFirstPage');
  {$endif}
  AInput2 := TMemoryStream.Create;
  AInput2.Size := AInput.Size;
  AInput2.CopyFrom(AInput, AInput.Size);
  AInput.Seek(0, soFromBeginning);
  AInput2.Seek(0, soFromBeginning);

  myAnLexicoPage := AnLexico.Create;
  myAnLexicoPage.Doc := AInput;
  myAnLexicoPage.bytesRemaining:= myAnLexicoPage.Doc.Size;
  myAnSintaticoPage := AnSintaticoPage.Create;

  // find first page
  while ((myAnSintaticoPage.pageFound <> true) and
                   (myAnLexicoPage.bytesRemaining > 0)) do
  begin
    mytoken := myAnLexicoPage.getToken();
    myAnSintaticoPage.automata(mytoken);
  end;

  if (myAnSintaticoPage.pageFound = false) then
  begin
    raise Exception.Create('ERROR: Arquivo corrompido.');
    Halt(1);
  end;

  AInput.Seek(0, soFromBeginning);
  myAnLexicoContents := AnLexico.Create;
  myAnLexicoContents.Doc := AInput;
  myAnLexicoContents.bytesRemaining:= myAnLexicoContents.Doc.Size;
  myAnSintaticoContents := AnSintaticoPageContents.Create;

  // gathering information of the first page
  myAnSintaticoContents.obj1:=myAnSintaticoPage.obj1;
  myAnSintaticoContents.obj2:=myAnSintaticoPage.obj2;

  //find first page contents
  while ((myAnSintaticoContents.contentsFound <> true) and
                   (myAnLexicoContents.bytesRemaining > 0)) do
  begin
    mytoken := myAnLexicoContents.getToken();
    myAnSintaticoContents.automata(mytoken, AInput2);
  end;

  if (myAnSintaticoContents.contentsFound = false) then
  begin
    raise Exception.Create('ERROR: Arquivo corrompido.');
    Halt(1);
  end;

  // gathering information of the first page
  myAnLexicoContents.bytesRemaining:=myAnSintaticoContents.h.page_length;

  // write file with content just from the first page
  while (myAnLexicoContents.bytesRemaining > 0) do
  begin
    mytoken := myAnLexicoContents.getPageToken();
    WriteStringToStream(AOutput, mytoken.token_string);
  end;

  Result:=myAnSintaticoContents.h;

  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':< TvPDFVectorialReader.getFirstPage');
  {$endif}

//  AInput2.Free;
end;

procedure TvPDFVectorialReader.unzipPage(AInput: TStream; AOutput: TStream);
var
  compr, uncompr: Pbyte;
  comprLen, uncomprLen: LongInt;
  myDecode: decode;
  BufStr: string;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':> TvPDFVectorialReader.unzipPage');
  {$endif}
  
  myDecode := Decode.Create;

  comprLen := 10000 * SizeOf(Integer); // don't overflow
  uncomprLen := comprLen;
  GetMem(compr, comprLen);
  GetMem(uncompr, uncomprLen);

  if (compr = NIL) or (uncompr = NIL) then
     myDecode.EXIT_ERR('Out of memory');

  (* compr and uncompr are cleared to avoid reading uninitialized
   * data and to ensure that uncompr compresses well.
   *)

  FillChar(compr^, comprLen, 0);
  FillChar(uncompr^, uncomprLen, 0);

  AInput.Read(compr^, comprLen);

  BufStr := string(myDecode.test_inflate(compr, comprLen, uncompr, uncomprLen));

  WriteStringToStream(AOutput, BufStr);

  FreeMem(compr, comprLen);
  FreeMem(uncompr, uncomprLen);

  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':< TvPDFVectorialReader.unzipPage');
  {$endif}
end;

procedure TvPDFVectorialReader.translatePage(AInput: TStream;
 AData: TvVectorialDocument; APageHeader: PageHeader);
var
  myAnLexico: AnLexico;
  myAnSintaticoCommand: AnSintaticoCommand;
  myAnSemantico: AnSemantico;
  mytoken: Token;
  c: Command;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':> TvPDFVectorialReader.translatePage');
  {$endif}

  // initialize data main
  myAnLexico := AnLexico.Create;
  myAnLexico.Doc := AInput;
  myAnLexico.bytesRemaining:= myAnLexico.Doc.Size;
  myAnSintaticoCommand := AnSintaticoCommand.Create;
  myAnSemantico := AnSemantico.Create;

  // initialize machine
  myAnSemantico.startMachine();

  while (myAnLexico.bytesRemaining > 0) do
  begin
    mytoken := myAnLexico.getToken();
    c:=myAnSintaticoCommand.automata(mytoken);
    if (myAnSintaticoCommand.Codigo = true) then
      myAnSemantico.generate(c, AData);
  end;

  // end machine
  myAnSemantico.endMachine();
end;

procedure TvPDFVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  APageHeader: PageHeader;
  APageStream, AUnzipStream: TStream;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':> TvPDFVectorialReader.ReadFromStream');
  {$endif}

  APageStream := TMemoryStream.Create;
  AUnzipStream := TMemoryStream.Create;

  // get first page
  APageHeader := getFirstPage(AStream, APageStream);

  // unzip page
  if (APageHeader.flate_decode = true) then
  begin
    APageStream.Seek(0, soFromBeginning);
    unzipPage(APageStream, AUnzipStream);

    // translate page to doc data
    AUnzipStream.Seek(0, soFromBeginning);
    translatePage(AUnzipStream, AData, APageHeader);
  end
  else
  begin
    // translate page to doc data
    APageStream.Seek(0, soFromBeginning);
    translatePage(APageStream, AData, APageHeader);
  end;

  APageStream.Free;
  AUnzipStream.Free;

  //ShowMessage('Sucesso!');
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':< TvPDFVectorialReader.ReadFromStream');
  WriteLn('Sucesso!');
  {$endif}
end;

{*******************************************************************
*  Initialization section
*
*  Registers this reader / writer on fpVectorial
*
*******************************************************************}
initialization

  RegisterVectorialReader(TvPDFVectorialReader, vfPDF);

end.

