{******************************************************************}
{*               IPMSG.PAS - MIME message classes                 *}
{******************************************************************}

{ $Id$ }

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Markus Kaemmerer <mk@happyarts.de> SourceForge: mkaemmerer
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }

{$I IPDEFINE.INC}

unit IpMsg;

interface

uses
  {$IFDEF IP_LAZARUS}
  LCLType,
  LCLIntf,
  FileUtil,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  IpStrms,
  {$IFNDEF IP_LAZARUS}
  //IpSock, //JMN
  {$ENDIF}
  IpUtils,
  IpConst;

type
  TIpMimeEncodingMethod = (em7Bit, em8Bit, emBase64, emBinary, emBinHex,
                           emQuoted, emUUEncode, emUnknown);


{ TIpMimeEntity }
type
  TIpCodingProgressEvent = procedure(Sender : TObject; Progress : Byte;
                                     var Abort : Boolean) of object;

{Begin !!.12}
type
  TIpHeaderTypes = (htBCC, htCC, htControl, htDate, htDispositionNotify,
                    htFollowUp, htFrom, htInReplyTo, htKeywords,
                    htMessageID, htNewsgroups, htNNTPPostingHost,
                    htOrganization, htPath, htPostingHost, htReceived,
                    htReferences, htReplyTo, htReturnPath, htSender,
                    htSubject, htTo, htUserFields, htXIpro);

  TIpHeaderInfo = record
    FieldType   : TIpHeaderTypes;
    FieldString : string;
  end;

const
  IpMaxHeaders = 24;

  IpHeaderXRef : array [0..IpMaxHeaders - 1] of  TIpHeaderInfo =
    ((FieldType : htBCC;               FieldString : 'BCC'),
     (FieldType : htCC;                FieldString : 'CC'),
     (FieldType : htControl;           FieldString : 'Control: '),
     (FieldType : htDate;              FieldString : 'Date'),
     (FieldType : htDispositionNotify; FieldString : 'Disposition-Notification-To'),
     (FieldType : htFollowUp;          FieldString : 'Followup-To: '),
     (FieldType : htFrom;              FieldString : 'From'),
     (FieldType : htInReplyTo;         FieldString : 'In-Reply-To'),
     (FieldType : htKeywords;          FieldString : 'Keywords'),
     (FieldType : htMessageID;         FieldString : 'Message-ID'),
     (FieldType : htNewsgroups;        FieldString : 'Newsgroups'),
     (FieldType : htNNTPPostingHost;   FieldString : 'NNTP-Posting-Host'),
     (FieldType : htOrganization;      FieldString : 'Organization'),
     (FieldType : htPath;              FieldString : 'Path'),
     (FieldType : htPostingHost;       FieldString : 'Posting-Host'),
     (FieldType : htReceived;          FieldString : 'Received'),
     (FieldType : htReferences;        FieldString : 'References'),
     (FieldType : htReplyTo;           FieldString : 'Reply-To'),
     (FieldType : htReturnPath;        FieldString : 'Return-Path'),
     (FieldType : htSender;            FieldString : 'Sender'),
     (FieldType : htSubject;           FieldString : 'Subject'),
     (FieldType : htTo;                FieldString : 'To'),
     (FieldType : htUserFields;        FieldString : 'X-'),
     (FieldType : htXIpro;             FieldString : 'X-Ipro'));

type
  TIpHeaderCollection = class;

  TIpHeaderItem = class (TCollectionItem)
    private
      FCollection  : TIpHeaderCollection;
      FName        : string;
      FNameL       : string;
        { Lower case version of FName. Used to speed up header searches. }
      FProperty    : Boolean;                                          {!!.13}
      FValue       : TStringList;
    protected
      procedure SetName(const Name : string);
      procedure SetValue (v : TStringList);
    public
      constructor Create (Collection : TCollection); override;
      destructor Destroy; override;
    published
      property Collection : TIpHeaderCollection
               read FCollection write FCollection;
      property Name : string read FName write SetName;
      property NameL : string read FNameL;
        { Lower case version of Name property. }
      property IsProperty : Boolean read FProperty write FProperty;    {!!.13}
        { Set to True if this header is exposed via an iPRO property. }{!!.13}
      property Value : TStringList read FValue write SetValue;
  end;

  TIpHeaderCollection = class (TCollection)
    private
      FOwner : TPersistent;                                              

    protected                                                            
      function GetItem (Index : Integer) : TIpHeaderItem;
      function GetOwner : TPersistent; override;                         
      procedure SetItem (Index : Integer; Value : TIpHeaderItem);        

    public                                                               
      constructor Create (AOwner : TPersistent);                         

      {$IFNDEF VERSION5}                                                 
      procedure Delete (Item : integer);                                 
      {$ENDIF}
      function HasHeader (AName : string) : Integer;
      procedure HeaderByName (AName   : string;
                              Headers : TStringList);
      procedure LoadHeaders (AHeaderList : TStringList;                  
                             Append      : Boolean);                     

      property Items[Index : Integer] : TIpHeaderItem                    
               read GetItem write SetItem;                               
  end;                                                                   
{End !!.12}

  TIpMimeParts = class; { Forwards }

  TIpMimeEntity = class(TPersistent)
  protected {private}
    FProgress                : Byte;
    PrevProgress             : Byte;
    FMimeParts               : TIpMimeParts;
    FParentBoundary          : string;
    FBody                    : TIpAnsiTextStream;
    FEntityName              : string;
    FBoundary                : string;
    FCharacterSet            : string;
    FContentDescription      : string;
    FContentDispositionType  : string;
    FContentID               : string;
    FContentSubtype          : string;
    FContentType             : string;
    FCreationDate            : string;
    FContentTransferEncoding : TIpMimeEncodingMethod;
    FFileName                : string;
    FIsMime                  : Boolean;
    FIsMultipart             : Boolean;
    FModificationDate        : string;
    FMimeVersion             : string;
    FOnCodingProgress        : TIpCodingProgressEvent;
    FOriginalSize            : Longint;
    FParent                  : TIpMimeEntity;
    FReadDate                : string;
    FRelatedType             : string;                                 {!!.02}
    FRelatedSubtype          : string;                                 {!!.02}
    FRelatedStart            : string;                                 {!!.02}
    FRelatedStartInfo        : string;                                 {!!.02}
    FAttachmentCount         : Integer;                                {!!.12}

  protected {methods}
    procedure Clear; virtual;
    procedure ClearBodyLargeAttach(const AttachmentSize : Longint); virtual;  {!!.12}
    function  ContainsSpecialChars(const Value : string) : Boolean;    {!!.14}
    procedure DecodeContentDisposition(const aDisp : string);
    procedure DecodeContentType(const aType : string);
    function  DecodeContentTransferEncoding(const aEncoding : string) :
                                            TIpMimeEncodingMethod;
    procedure DecodeMimeHeaders(RawHeaders : TStringlist);
    procedure DoOnCodingProgress(Count, TotalSize : Longint; var Abort : Boolean);
    procedure EncodeContentDisposition(RawHeaders : TStringList);
    procedure EncodeContentType(RawHeaders : TStringList);
    procedure EncodeContentTransferEncoding(RawHeaders : TStringList);
    procedure EncodeMimeHeaders(RawHeaders : TStringlist);
    procedure OctetStreamToHextetStream(InStream : TStream; OutStream : TIpAnsiTextStream;
                                        const Table; PadChar, Delim : AnsiChar);
    procedure Decode8Bit(OutStream : TStream);
    procedure DecodeBase64(OutStream : TStream);
    procedure DecodeBinHex(OutStream : TStream);
    procedure DecodeQuoted(OutStream : TStream);
    procedure DecodeUUEncode(OutStream : TStream);
    procedure Encode8Bit(InStream : TStream);
    procedure EncodeBase64(InStream : TStream);
    procedure EncodeBinHex(InStream : TStream; const aFileName : string);
    procedure EncodeQuoted(InStream : TStream);
    procedure EncodeUUEncode(InStream : TStream; const aFileName : string);
    function DecodeEntity(InStream : TIpAnsiTextStream) : string;
    function DecodeEntityAsAttachment(InStream : TIpAnsiTextStream) : string;  {!!.01}
    function EncodeEntity(OutStream : TIpAnsiTextStream) : string;
    procedure ReadBody(InStream : TIpAnsiTextStream; const StartLine : string); {!!.12}

  protected {properties}
    property OnCodingProgress : TIpCodingProgressEvent
      read FOnCodingProgress write FOnCodingProgress;

  public {methods}
    constructor Create(ParentEntity : TIpMimeEntity); virtual;
    destructor  Destroy; override;
    procedure ClearBody;
    procedure EncodeBodyFile(const InFile : string);
    procedure EncodeBodyStream(InStream : TStream; const aFileName : string);
    procedure EncodeBodyStrings(InStrings : TStrings; const aFileName : string);
    procedure ExtractBodyFile(const OutFile : string);
    procedure ExtractBodyStream(OutStream : TStream);
    procedure ExtractBodyStrings(OutStrings : TStrings);
    function FindNestedMimePart(const aType, aSubType, aContentID : string) : TIpMimeEntity; {!!.02}
    function  GetMimePart(const aType, aSubType, aContentID : string;
                              CanCreate : Boolean) : TIpMimeEntity;
    function  NewMimePart : TIpMimeEntity;

    property AttachmentCount : Integer read FAttachmentCount;          {!!.12}

  public {properties}
    property Body : TIpAnsiTextStream
      read FBody;

    property Boundary : string
      read FBoundary write FBoundary;

    property CharacterSet : string
      read FCharacterSet write FCharacterSet;

    property ContentDescription : string
      read FContentDescription write FContentDescription;

    property ContentDispositionType : string
      read FContentDispositionType write FContentDispositionType;

    property ContentID : string
      read FContentID write FContentID;

    property ContentSubtype : string
      read FContentSubtype write FContentSubtype;

    property ContentTransferEncoding : TIpMimeEncodingMethod
      read FContentTransferEncoding write FContentTransferEncoding;

    property ContentType : string
      read FContentType write FContentType;

    property CreationDate : string
      read FCreationDate write FCreationDate;

    property EntityName : string
      read FEntityName write FEntityName;

    property FileName : string
      read FFileName write FFileName;

    property IsMime : Boolean
      read FIsMime;

    property IsMultipart : Boolean
      read FIsMultipart;

    property MimeParts : TIpMimeParts
      read FMimeParts;

    property MimeVersion : string
      read FMimeVersion write FMimeVersion;

    property ModificationDate : string
      read FModificationDate write FModificationDate;

    property OriginalSize : Longint
      read FOriginalSize write FOriginalSize;

    property Parent : TIpMimeEntity
      read FParent;

    property ReadDate : string
      read FReadDate write FReadDate;

    property RelatedStart : string                                   {!!.02}
      read FRelatedStart write FRelatedStart;

    property RelatedStartInfo : string                               {!!.02}
      read FRelatedStartInfo write FRelatedStartInfo;

    property RelatedSubtype : string                                 {!!.02}
      read FRelatedSubtype write FRelatedSubtype;

    property RelatedType : string                                    {!!.02}
      read FRelatedType write FRelatedType;

  end;


{ TIpMimeParts }
  TIpMimeParts = class
  protected {private}
    Entitys : TList;
    function GetCount : Integer;
    function GetPart(aIndex : Integer) : TIpMimeEntity;
  public {methods}
    constructor Create;
    destructor  Destroy; override;
    function Add(aEntity : TIpMimeEntity) : Integer;
    function Remove(aEntity : TIpMimeEntity) : Integer;
    procedure Clear;
    procedure Delete(aIndex : Integer);
    function IndexOf(aEntity : TIpMimeEntity) : Integer;
  public {properties}
    property Count : Integer
      read GetCount;
    property Parts[aIndex : Integer] : TIpMimeEntity
      read GetPart; default;
  end;


{ TIpMessage }
type
  TIpMessage = class(TIpMimeEntity)
  protected {private}
    MsgStream : TIpAnsiTextStream;

  protected {property variables}
    FBCC             : TStringList;
    FCC              : TStringList;
    FDate            : string;
    FFrom            : string;
    FInReplyTo       : string;
    FKeywords        : string;
    FFollowupTo      : string;                                           {!!.12}
    FControl         : string;                                           {!!.12}
    FMessageID       : string;
    FMessageTag      : Integer;
    FNewsgroups      : TStringList;
    FNNTPPostingHost : string;
    FOrganization    : string;
    FPath            : TStringList;
    FPostingHost     : string;
    FReceived        : TStringList;
    FRecipients      : TStringList;
    FReferences      : TStringList;
    FReplyTo         : string;
    FReturnPath      : string;
    FSender          : string;
    FSubject         : string;
    FUserFields      : TStringList;
    FHeaders         : TIpHeaderCollection;                              {!!.12}
    FDispositionNotify: string;

  protected {methods}
    procedure CheckAllHeaders;                                           {!!.12}
    procedure CheckHeaderType (HeaderInfo : TIpHeaderItem;               {!!.12}
                               HeaderType : TIpHeaderTypes);             {!!.12}
    procedure Clear; override;
    procedure NewMessageStream;
    function  GetPosition : Longint;
    function  GetSize : Longint;
    procedure SetPosition(Value : Longint);
    procedure SetBCC(const Value: TStringList);
    procedure SetCC(const Value: TStringList);
    procedure SetNewsgroups(const Value: TStringList);
    procedure SetPath(const Value: TStringList);
    procedure SetReceived(const Value: TStringList);
    procedure SetRecipients(const Value: TStringList);
    procedure SetReferences(const Value: TStringlist);
    procedure SetUserFields(const Value: TStringList);

  public {methods}
    constructor CreateMessage; virtual;
    destructor  Destroy; override;

    procedure AddDefaultAttachment(const aFileName : string);          {!!.02}
    procedure AddDefaultAttachmentAs (const aFileName      : string;   {!!.12}
                                      const AttachmentName : string);  {!!.12}
    procedure Assign(Source : TPersistent); override;
    function  AtEndOfStream : Boolean;
    procedure DecodeMessage; virtual;
    procedure EncodeMessage; virtual;
    function  GetBodyHtml(CanCreate : Boolean) : TIpMimeEntity;
    function  GetBodyPlain(CanCreate : Boolean) : TIpMimeEntity;
    procedure LoadFromFile(const aFileName : string);
    procedure LoadFromStream(aStream : TStream);                       {!!.12}
    procedure NewMessage;
    function  ReadLine : string;
    function  ReadLineCRLF : string;
    procedure SaveToFile(const aFileName : string);
    procedure SaveToStream(Stream: TStream);                           {!!.12}
    procedure SetHeaders(Headers : TIpHeaderCollection);               {!!.12}
    procedure WriteLine(const aSt : string);

  public {properties}
    property BCC : TStringList
      read FBCC write SetBCC;                                          {!!.01}

    property CC : TStringList
      read FCC write SetCC;                                            {!!.01}

    property Control : string                                          {!!.12}
      read FControl write FControl;                                    {!!.12}

    property Date : string
      read FDate write FDate;

    property DispositionNotification : string                          {!!.12}
      read FDispositionNotify write FDispositionNotify;                {!!.12}

    property FollowupTo : String                                       {!!.12}
      read FFollowupTo Write FFollowupTo;                              {!!.12}

    property From : string
      read FFrom write FFrom;

    property Headers : TIpHeaderCollection                             {!!.12}
             read FHeaders write SetHeaders;                           {!!.12}

    property InReplyTo : string
      read FInReplyTo write FInReplyTo;

    property Keywords : string
      read FKeywords write FKeywords;

    property MessageID : string
      read FMessageID write FMessageID;

    property MessageStream : TIpAnsiTextStream                         {!!.03}
      read MsgStream;                                                  {!!.03}

    property MessageTag : Integer
      read FMessageTag write FMessageTag;

    property Newsgroups : TStringList
      read FNewsgroups write SetNewsgroups;                            {!!.01}

    property NNTPPostingHost : string
      read FNNTPPostingHost write FNNTPPostingHost;

    property Organization : string
      read FOrganization write FOrganization;

    property Path : TStringList
      read FPath write SetPath;                                        {!!.01}

    property Position : Longint
      read GetPosition write SetPosition;

    property PostingHost : string
      read FPostingHost write FPostingHost;

    property Received : TStringList
      read FReceived write SetReceived;                                {!!.01}

    property Recipients : TStringList
      read FRecipients write SetRecipients;                            {!!.01}

    property References : TStringlist
      read FReferences write SetReferences;                            {!!.01}

    property ReplyTo : string
      read FReplyTo write FReplyTo;

    property ReturnPath : string
      read FReturnPath write FReturnPath;

    property Sender : string
      read FSender write FSender;

    property Size : Longint
      read GetSize;

    property Subject : string
      read FSubject write FSubject;

    property UserFields : TStringList
      read FUserFields write SetUserFields;                            {!!.01}

  end;


{ TIpMailMessage}
type
  TIpMailMessage = class(TIpMessage)
  published {properties}
    property BCC;
    property CC;
    property ContentDescription;
    property ContentTransferEncoding;
    property ContentType;
    property Date;
    property From;
    property Keywords;
    property MailTo : TStringList
      read FRecipients write SetRecipients;                            {!!.01}
    property OnCodingProgress;
    property References;
    property ReplyTo;
    property Sender;
    property Subject;
    property UserFields;
end;


{ TIpNewsArticle }
type
  TIpNewsArticle = class(TIpMessage)
  published {properties}
    property ContentDescription;
    property ContentTransferEncoding;
    property ContentType;
    property Date;
    property From;
    property Keywords;
    property Newsgroups;
    property NNTPPostingHost;
    property OnCodingProgress;
    property Path;
    property References;
    property ReplyTo;
    property Sender;
    property Subject;
    property UserFields;
end;


{ TIpFormDataEntity }
type
  TIpFormDataEntity = class(TIpMimeEntity)
  protected
    FFilesEntity : TIpMimeEntity;
  public {methods}
    constructor Create(ParentEntity : TIpMimeEntity); override;
    destructor  Destroy; override;
    procedure AddFormData(const aName, aText : string);
    procedure AddFile(const aFileName, aContentType, aSubtype : string;
                      aEncoding : TIpMimeEncodingMethod);
    procedure SaveToStream(aStream : TStream);
  end;

 {$IFNDEF IP_LAZARUS}
 { dummy class so this unit will be added to the uses clause when an }
 { IpPop3Client, IpSmtpClient or IpNntpClient component is dropped on the form }
 (*** //JMN
 TIpCustomEmailClass = class(TIpCustomClient)
 end;
 **)
 {$ENDIF}

function IpBase64EncodeString(const InStr: string): string;       {!!.02}{!!.03}

{Begin !!.12}
const
  IpLgAttachSizeBoundry = 5 * 1024 * 1024;
    { Attachments over this size will be encoded using a TIpMemMapStream for
      greatly improved performance. This boundary also applies to the final
      encoding of messages with large attachments. }

implementation

const
  { standard headers }
  strBCC               = 'BCC: ';
  strCC                = 'CC: ';
  strDate              = 'Date: ';
  strDispositionNotify = 'Disposition-Notification-To: ';
  strFrom              = 'From: ';
  strInReplyTo         = 'In-Reply-To: ';
  strKeywords          = 'Keywords: ';
  strMessageID         = 'Message-ID: ';
  strNewsgroups        = 'Newsgroups: ';
  strNNTPPostingHost   = 'NNTP-Posting-Host: ';
  strOrganization      = 'Organization: ';
  strPath              = 'Path: ';
  strPostingHost       = 'Posting-Host: ';
  strReceived          = 'Received: ';
  strReferences        = 'References: ';
  strReplyTo           = 'Reply-To: ';
  strReturnPath        = 'Return-Path: ';
  strSender            = 'Sender: ';
  strSubject           = 'Subject: ';
  strTo                = 'To: ';
  strUserFields        = 'X-';
  strXIpro             = 'X-Ipro: ';
  strFollowUp          = 'Followup-To: ';                               {!!.12}
  strControl           = 'Control: ';                                   {!!.12}

{Begin !!.13}
  IpMimeHeaders : array [0..5] of string =
    { List of MIME headers that must be marked as public properties in
      the message's Headers collection. Marking them as a public property
      prevents them from being written out twice if the message is saved
      to a file or stream. }
    (
      'Content-Type',
      'MIME-Version',
      'Content-Transfer-Encoding',
      'Content-Description',
      'Content-ID',
      'Content-Disposition'
    );
{End !!.13}

  { MIME headers }
  strMimeVersion             = 'MIME-Version: ';
  strContent                 = 'Content-';
  strContentBase             = strContent + 'Base: ';
  strContentDescription      = strContent + 'Description: ';
  strContentDisposition      = strContent + 'Disposition: ';
  strContentID               = strContent + 'ID: ';
  strContentLanguage         = strContent + 'Language: ';
  strContentLocation         = strContent + 'Location: ';
  strContentTransferEncoding = strContent + 'Transfer-Encoding: ';
  strContentType             = strContent + 'Type: ';

  { MIME content types }
  strApplication = 'application';
  strAudio       = 'audio';
  strFiles       = 'files';
  strFormData    = 'form-data';
  strImage       = 'image';
  strMessage     = 'message';
  strMultiPart   = 'multipart';
  strText        = 'text';
  strVideo       = 'video';

  { MIME content subtypes and parameters }
  strBoundary    = 'boundary=';
  strCharSet     = 'charset=';
  strMixed       = 'mixed';
  strName        = 'name=';
  strPlain       = 'plain';
  strHTML        = 'html';
  strOctetStream = 'octet-stream';
  strAlternative = 'alternative';
  strRelated     = 'related';                                        {!!.02}

  { MIME content disposition parameters }
  strAttachment       = 'attachment';
  strInline           = 'inline';
  strCreationDate     = 'creation-date=';
  strFilename         = 'filename=';
  strModificationDate = 'modification-date=';
  strReadDate         = 'read-date=';
  strStart            = 'start=';                                    {!!.02}
  strStartInfo        = 'start-info=';                               {!!.02}
  strSize             = 'size=';
  strType             = 'type=';                                     {!!.02}


  { MIME encoding methods }
  str7Bit     = '7bit';
  str8Bit     = '8bit';
  strBase64   = 'base64';
  strBinary   = 'binary';
  strBinHex   = 'binhex';
  strQuoted   = 'quoted-printable';
  strUUEncode = 'uuencoded';


  { default MIME content type information }
{$I IPDEFCT.INC}

type
  TIp6BitTable = array[0..63] of AnsiChar;

const {- BinHex encoding table }
  IpBinHexTable : TIp6BitTable = (
    '!', '"', '#', '$', '%', '&', '''', '(',
    ')', '*', '+', ',', '-', '0', '1',  '2',
    '3', '4', '5', '6', '8', '9', '@',  'A',
    'B', 'C', 'D', 'E', 'F', 'G', 'H',  'I',
    'J', 'K', 'L', 'M', 'N', 'P', 'Q',  'R',
    'S', 'T', 'U', 'V', 'X', 'Y', 'Z',  '[',
    '`', 'a', 'b', 'c', 'd', 'e', 'f',  'h',
    'i', 'j', 'k', 'l', 'm', 'p', 'q',  'r');

const {-BinHex decoding table }
  IpHexBinTable : array[33..114] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07,
    $08, $09, $0A, $0B, $0C, $FF, $FF, $0D,
    $0E, $0F, $10, $11, $12, $13, $FF, $14,
    $15, $FF, $FF, $FF, $FF, $FF, $FF, $16,
    $17, $18, $19, $1A, $1B, $1C, $1D, $1E,
    $1F, $20, $21, $22, $23, $24, $FF, $25,
    $26, $27, $28, $29, $2A, $2B, $FF, $2C,
    $2D, $2E, $2F, $FF, $FF, $FF, $FF, $30,
    $31, $32, $33, $34, $35, $36, $FF, $37,
    $38, $39, $3A, $3B, $3C, $FF, $FF, $3D,
    $3E, $3F);

const { Base64 encoding table }
  Ip64Table : TIp6BitTable = (
    #065, #066, #067, #068, #069, #070, #071, #072,
    #073, #074, #075, #076, #077, #078, #079, #080,
    #081, #082, #083, #084, #085, #086, #087, #088,
    #089, #090, #097, #098, #099, #100, #101, #102,
    #103, #104, #105, #106, #107, #108, #109, #110,
    #111, #112, #113, #114, #115, #116, #117, #118,
    #119, #120, #121, #122, #048, #049, #050, #051,
    #052, #053, #054, #055, #056, #057, #043, #047);

const { Base64 decoding table }
  IpD64Table : array[#43..#122] of Byte = (                          {!!.12}
    $3E, $7F, $7F, $7F, $3F, $34, $35, $36,
    $37, $38, $39, $3A, $3B, $3C, $3D, $7F,
    $7F, $7F, $7F, $7F, $7F, $7F, $00, $01,
    $02, $03, $04, $05, $06, $07, $08, $09,
    $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
    $12, $13, $14, $15, $16, $17, $18, $19,
    $7F, $7F, $7F, $7F, $7F, $7F, $1A, $1B,
    $1C, $1D, $1E, $1F, $20, $21, $22, $23,
    $24, $25, $26, $27, $28, $29, $2A, $2B,
    $2C, $2D, $2E, $2F, $30, $31, $32, $33);

const { UUEncode encoding table }
  IpUUTable : TIp6BitTable = (
    #96, #33, #34, #35, #36, #37, #38, #39,
    #40, #41, #42, #43, #44, #45, #46, #47,
    #48, #49, #50, #51, #52, #53, #54, #55,
    #56, #57, #58, #59, #60, #61, #62, #63,
    #64, #65, #66, #67, #68, #69, #70, #71,
    #72, #73, #74, #75, #76, #77, #78, #79,
    #80, #81, #82, #83, #84, #85, #86, #87,
    #88, #89, #90, #91, #92, #93, #94, #95);

const
  HexDigits : array[0..$F] of AnsiChar = '0123456789ABCDEF';
  RLEChar : Byte = $90;
  BinHexFileType : array[0..3] of Byte = ($49, $42, $4D, $3F);  { "IBM?" }
  CRLF = #13#10;
  MaxLine = 1000;                                                       {!!.12}
  MaxLineEncode = 77;                                                   {!!.13}
    { Maximum line length for QuotablePrintable & Base64 encoding. }    {!!.13}

type
  BinHexHeader = packed record
    Version  : Byte;
    FileType : array[0..3] of Byte;
    Creator  : array[0..3] of Byte;
    Flags    : Word;
    DFLong   : Longint;
    RFLong   : Longint;
  end;

function IsSameString (Str1          : string;                           {!!.12}
                       Str2          : string;                           {!!.12}
                       CaseSensitive : Boolean) : Boolean;               {!!.12}
begin                                                                    {!!.12}
  if CaseSensitive then                                                  {!!.12}
    Result := (Str1 = Str2)                                              {!!.12}
  else                                                                   {!!.12}
    Result := StrIComp (PChar (Str1), PChar (Str2)) = 0;                 {!!.12}
end;                                                                     {!!.12}

{ Parse string into string list }
procedure Parse(const Line : string; Delim : AnsiChar; var List : TStringList);
var
  iPos, jPos : Integer;
  Term : string;
begin
  iPos := 1;
  jPos := IpUtils.CharPos(Delim, Line);
  while (jPos > 0) do begin
    Term := Copy(Line, iPos, jPos - iPos);                           {!!.02}
    if (Term <> '') then
      List.Add(Trim(Term));
    iPos := jPos + 1;
    jPos := IpUtils.CharPosIdx(Delim, Line, iPos);
  end;
  if (iPos < Length(Line)) then
     List.Add(Trim(Copy(Line, iPos, Length(Line))));
end;

{ Return a particular parameter from a parsed header parameter list }
procedure DecodeSingleParameter(const ParamName : string;
                                RawParams : TStringList;
                                var ParamFieldStr : string);
var
  S : string;
  i, j : Integer;
begin
  ParamFieldStr := '';
  {find the line containing the parameter field name}
  for i := 1 to RawParams.Count do begin
    S := RawParams[i-1];
    if StrLIComp(PChar(ParamName), PChar(S), Length(ParamName)) = 0 then begin
      {strip off the parameter field name and remove quotes }
      ParamFieldStr := Copy(S, Length(ParamName) + 1, Length(S));
      j := IpUtils.CharPos('"', ParamFieldStr);
      while (j > 0) do begin
        Delete(ParamFieldStr, j, 1);
        j := IpUtils.CharPos('"', ParamFieldStr);
      end;
      Break;
    end;
  end;
end;

{ Return a particular header as string }
procedure DecodeSingleHeader(const HeaderName : string;
                             RawHeaders : TStringList;
                             var HeaderFieldStr : string);
var
  S, S2 : string;
  i, j : Integer;
begin
  HeaderFieldStr := '';
  {find the line containing the header field name}
  for i := 1 to RawHeaders.Count do begin
    S := RawHeaders[i-1];
    if StrLIComp(PChar(HeaderName), PChar(S), Length(HeaderName)) = 0 then begin
      {strip off the header field name}
      S := Copy(S, Length(HeaderName) + 1, Length(S));
      {unfold the header if continued on more than one line}
      if (i < RawHeaders.Count) then
        for j := i to Pred(RawHeaders.Count) do begin
          S2 := RawHeaders[j];
          if (Length(S2) > 0) and (S2[1] <> #09) and (S2[1] <> ' ') then
            Break
          else
            S := S + S2;
        end;
      HeaderFieldStr := S;
      Break;
    end;
  end;
end;

{ Return a particular header as string list }
(*procedure DecodeListHeader(const HeaderName : string;
                           RawHeaders, HeaderFieldList : TStringList);
var
  S : string;
  i, j : Integer;
begin
  {find the line containing the header field name}
  for i := 1 to RawHeaders.Count do begin
    S := RawHeaders[i-1];
    if StrLIComp(PChar(HeaderName), PChar(S), Length(HeaderName)) = 0 then begin
      {strip off the header field name}
      HeaderFieldList.Add(Copy(S, Length(HeaderName) + 1, Length(S)));
      {unfold the header if continued on more than one line}
      if (i < RawHeaders.Count) then
        for j := i to Pred(RawHeaders.Count) do begin
          S := RawHeaders[j];
          if (Length(S) > 0) and (S[1] <> #09) and (S[1] <> ' ') then
            Break
          else
            HeaderFieldList.Add(S);
        end;
      Break;
    end;
  end;
end;*)

{ Return multiple instance headers as string list }
(*procedure DecodeMultiHeader(const HeaderName : string;
                            RawHeaders, HeaderFieldList : TStringList);

var
  S, S2 : string;
  i, j : Integer;
begin
  {find the next line containing the header field name}
  for i := 1 to RawHeaders.Count do begin
    S := RawHeaders[i-1];
    if StrLIComp(PChar(HeaderName), PChar(S), Length(HeaderName)) = 0 then begin
      if HeaderName <> strUserFields then begin                         {!!.11}
        {strip off the header field name}
        S := Copy(S, Length(HeaderName) + 1, Length(S));
        {unfold the header if continued on more than one line}
        if (i < RawHeaders.Count) then
          for j := i to Pred(RawHeaders.Count) do begin
            S2 := RawHeaders[j];
            if (Length(S2) > 0) and (S2[1] <> #09) and (S2[1] <> ' ') then
              Break
            else
              S := S + S2;
          end;
      end;                                                              {!!.11}
      HeaderFieldList.Add(S);
    end;
  end;
end;*)

{ Add header string to raw headers }
procedure EncodeSingleHeader(const HeaderName : string;
                             RawHeaders : TStringList;
                             HeaderFieldStr : string);
begin
  if (HeaderFieldStr <> '') then
    RawHeaders.Add(HeaderName + HeaderFieldStr);
end;

{ Unfold multiple line header and add to raw headers }
procedure EncodeListHeader(const HeaderName : string;
                           RawHeaders, HeaderFieldList : TStringList;
                           const Delim : string;
                           Fold : Boolean);
var
  S : string;
  i : Integer;
begin
  if (HeaderFieldList.Count > 0) then begin
    S := HeaderName;
    for i := 0 to Pred(HeaderFieldList.Count) do begin
      if (Length(S + HeaderFieldList[i]) > MaxLine) then begin
        RawHeaders.Add(S);
        S := #09;
      end;
      S := S + HeaderFieldList[i];
      if (i < HeaderFieldList.Count - 1) and (S <> '') then begin
        S := S + Delim;                                                {!!.14}
        if Fold then begin
          RawHeaders.Add(S);
          S := #09;
        end;
      end;
    end;
    RawHeaders.Add(S);
  end;
end;

{ Add multiple instance header to raw headers }
procedure EncodeMultiHeader(const HeaderName : string;
                            RawHeaders, HeaderFieldList : TStringList;
                            Delim : AnsiChar;
                            Fold : Boolean);
var
  i, j : Integer;
  SL : TStringList;
  S : string;
begin
  if (HeaderFieldList.Count > 0) then
    for j := 1 to HeaderFieldList.Count do begin
      if not Fold then
        RawHeaders.Add(HeaderName + HeaderFieldList[j-1])
      else begin
        SL := TStringList.Create;
        try
          Parse(HeaderFieldList[j-1], Delim, SL);
          S := HeaderName;
          for i := 1 to SL.Count do begin
            S := S + SL[i-1];
            if (i < SL.Count) and (S <> '') then begin
{Begin !!.13}
              RawHeaders.Add(S);
              S := Delim;
{End !!.13}
            end;
          end;
        finally
          SL.Free;
        end;
        RawHeaders.Add(S);
      end;
    end;
end;

{ Generate "unique" boundary string }
function GenerateBoundary : string;
var
  Temp : TDateTime;
begin
  Temp := Now;
  Randomize;
  Result := '_NextPart_' + IntToHex(Trunc(Temp), 8) + '-' +
    IntToHex(Trunc(Frac(Temp) * 10000), 8) + '-' +
    IntToHex(GetTickCount, 8) + '-' + IntToHex(Random($FFFF), 4);
end;

{ 16-bit CRC of stream between starting and ending offset }
function BinHexCRC(Stream : TStream; StartOffset, EndOffset : Longint) : Word;
var
  Crc : Word;
  InByte : Byte;
  ByteStream : TIpByteStream;

  procedure DoCRC(b : Byte);
    {- carry CRC division on with next byte }
  var
    j : Byte;
    t : Boolean;
  begin
    for j := 1 to 8 do begin
      t := (Crc and $8000) <> 0;
      Crc := (Crc shl 1) xor (b shr 7);
      if t then
        Crc := Crc xor $1021;
      b := b shl 1;
    end;
  end;

begin
  if (StartOffset > Stream.Size) or (EndOffset > Stream.Size) then
    raise EIpBaseException.Create(SBadOffset);

  Crc := 0;
  Stream.Position := StartOffset;
  ByteStream := TIpByteStream.Create(Stream);
  try
    while (ByteStream.Position < EndOffset) do begin
      if ByteStream.Read(InByte) then
        DoCrc(InByte);
    end;
  finally
    ByteStream.Free;
  end;
  DoCrc(0);
  DoCrc(0);
  Result := Swap(Crc);
end;

{ Reverse bytes and words }
function htonl(HostLong : Longint) : Longint;
var
  dw : Longint;
  wa : array[0..1] of Word absolute dw;
  w  : Word;
begin
  dw := HostLong;
  w := wa[0];
  wa[0] := Swap(wa[1]);
  wa[1] := Swap(w);
  Result := dw;
end;

{Begin !!.12}
{ TIpHeaderItem ****************************************************** }

constructor TIpHeaderItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TIpHeaderCollection.Create (
                     TIpHeaderCollection(Collection).FOwner);

  FValue := TStringList.Create;
  FName  := '';
  FProperty := False;                                                  {!!.13}
end;                                                                     

destructor TIpHeaderItem.Destroy;                                        
begin                                                                    
  FCollection.Free;                                                      
  FCollection := nil;                                                    

  FValue.Free;                                                           
  FValue := nil;                                                         

  inherited Destroy;                                                     
end;                                                                     

procedure TIpHeaderItem.SetName(const Name : string);
begin
  FName := Name;
  FNameL := LowerCase(Name);
end;

procedure TIpHeaderItem.SetValue (v : TStringList);                      
begin                                                                    
  FValue.Assign (v);                                                     
end;                                                                     

{ TIpHeaderCollection ************************************************ } 

constructor TIpHeaderCollection.Create(AOwner : TPersistent);            
begin                                                                    
  inherited Create (TIpHeaderItem);                                      
  FOwner := AOwner;                                                      
end;                                                                     

{$IFNDEF VERSION5}                                                       
procedure TIpHeaderCollection.Delete(Item: integer);                     
begin                                                                    
  GetItem(Item).Free;                                                    
end;                                                                     
{$ENDIF}                                                                 

function TIpHeaderCollection.GetItem (Index : Integer) : TIpHeaderItem;  
begin                                                                    
  Result := TIpHeaderItem (inherited GetItem (Index));                   
end;                                                                     

function TIpHeaderCollection.GetOwner : TPersistent;                     
begin                                                                    
  Result := FOwner;                                                      
end;                                                                     

function TIpHeaderCollection.HasHeader (AName : string) : Integer;
var                                                                      
  i : Integer;                                                           
begin                                                                    
  Result := -1;                                                          
  AName := LowerCase(AName);
  for i := 0 to Count - 1 do
    if Items[i].NameL = AName then begin                      
      Result := i;                                                       
      Break;                                                             
    end;                                                                 
end;                                                                     

procedure TIpHeaderCollection.HeaderByName (AName   : string;            
                                            Headers : TStringList);      
var
  HeaderPos : Integer;                                                   
begin                                                                    
  Headers.Clear;                                                         
  HeaderPos := HasHeader (AName);                                        
  if HeaderPos >= 0 then                                                 
    Headers.Assign (Items[HeaderPos].Value);                             
end;                                                                     

procedure TIpHeaderCollection.LoadHeaders (AHeaderList : TStringList;
                                           Append      : Boolean);
var
  CurPos : Integer;

  function ExtractHeaderName (const AName : string) : string;
  {!!.15 - replaced local variable i with inx in order to avoid confusion with
    variable i in parent routine. }
  var
    inx     : Integer;
    NameLen : Integer;
  begin
    Result := '';
    CurPos := 0;

    inx := 0;
    NameLen := Length (AName);
    while (inx < NameLen) and (AName[inx + 1] <> ':') and
          (AName[inx + 1] >= #33) and (AName[inx + 1] <= #126) do
      Inc (inx);
    if (inx > 0) then
      Result := Copy (AName, 1, inx);
    CurPos := inx + 2;
  end;

  function IsWrappedLine (AHeaderList : TStringList;
                          LineToCheck : Integer) : Boolean;
  begin
    if LineToCheck < AHeaderList.Count then begin
      if Length (AHeaderList[LineToCheck]) > 0 then begin
        if (AHeaderList[LineToCheck][1] = ' ') or
           (AHeaderList[LineToCheck][1] = #09) then
          Result := True
        else
          Result := False;
      end else
        Result := False;
    end else
      Result := False;
  end;

  procedure GetFieldValue (    AHeaderList : TStringList;
                           var CurLine     : Integer;
                           var NewField    : TIpHeaderItem);
  var
    WorkLine : string;
    LineLen  : Integer;

  begin
    if CurLine >= AHeaderList.Count then
      Exit;
    LineLen  := Length (AHeaderList[CurLine]);
    while (CurPos < LineLen) and
          ((AHeaderList[CurLine][CurPos] = ' ') or
           (AHeaderList[CurLine][CurPos] = #09)) do
      Inc (CurPos);
    WorkLine := Copy (AHeaderList[CurLine],
                      CurPos, LineLen - CurPos + 1);
{Begin !!.13}
    Inc(CurLine);

    while IsWrappedLine (AHeaderList, CurLine) do begin
      WorkLine := WorkLine + #9 + Trim(AHeaderList[CurLine]);
      Inc(CurLine);
    end;
    NewField.Value.Add (Trim (WorkLine));
{End !!.13}
  end;

var                                                                      
  i          : Integer;                                                  
  HeaderName : string;                                                   
  NewHeader  : TIpHeaderItem;                                            
begin                                                                    
  if not Append then                                                     
    Clear;

  i := 0;                                                                
  while i < AHeaderList.Count do begin                                   
    HeaderName := ExtractHeaderName (AHeaderList[i]);                    
    if HeaderName <> '' then begin                                       
      NewHeader := TIpHeaderItem (Add);                                  
      NewHeader.Name := HeaderName;                                      
      GetFieldValue (AHeaderList, i, NewHeader);
{Begin !!.15}
    end
    else
      Inc(i);
{End !!.15}
  end;
end;                                                                     

procedure TIpHeaderCollection.SetItem (Index : Integer;                  
                                       Value : TIpHeaderItem);           
begin                                                                    
  inherited SetItem (Index, Value);                                      
end;
{End !!.12}

{ TIpMimeParts }
constructor TIpMimeParts.Create;
begin
  inherited Create;
  Entitys := TList.Create;
end;

destructor TIpMimeParts.Destroy;
begin
  Clear;
  Entitys.Free;
  inherited Destroy;
end;

{ Add Mime block to list }
function TIpMimeParts.Add(aEntity : TIpMimeEntity) : Integer;
begin
  Result := Entitys.Add(aEntity);
end;

{ Clear list }
procedure TIpMimeParts.Clear;
var
  i : Integer;
begin
  for i := Pred(Entitys.Count) downto 0 do
    Delete(i);
end;

{ Delete block from list }
procedure TIpMimeParts.Delete(aIndex : Integer);
begin
  if (aIndex >= 0) and (aIndex < Entitys.Count) then begin
    TIpMimeEntity(Entitys[aIndex]).Free;
  end;
end;

{ Remove block from list }
function TIpMimeParts.Remove(aEntity : TIpMimeEntity) : Integer;
begin
  Result := Entitys.Remove(Pointer(aEntity));
end;

{ Count property read access method }
function TIpMimeParts.GetCount : Integer;
begin
  Result := Entitys.Count;
end;

{ Parts property read access method }
function TIpMimeParts.GetPart(aIndex : Integer) : TIpMimeEntity;
begin
  if (aIndex >= 0) and (aIndex < Entitys.Count) then
    Result := TIpMimeEntity(Entitys[aIndex])
  else
    Result := nil;
end;

{ Returns list index of specified Mime block }
function TIpMimeParts.IndexOf(aEntity : TIpMimeEntity) : Integer;
begin
  Result := Entitys.IndexOf(aEntity);
end;


{ TIpMimeEntity }
constructor TIpMimeEntity.Create(ParentEntity : TIpMimeEntity);
begin
  inherited Create;
  FBody := TIpAnsiTextStream.CreateEmpty;
  FBody.Stream := TMemoryStream.Create;
  FMimeParts := TIpMimeParts.Create;
  FParent := ParentEntity;
  if (FParent <> nil) then
    FParentBoundary := FParent.Boundary;
end;

destructor TIpMimeEntity.Destroy;
begin
  FMimeParts.Free;
  FBody.FreeStream;
  FBody.Free;
  if (FParent <> nil) then
    FParent.MimeParts.Remove(Self);
  inherited Destroy;
end;

{ Clear Body property }
procedure TIpMimeEntity.ClearBody;
begin
  FBody.FreeStream;
  FBody.Stream := TMemoryStream.Create;
end;

{Begin !!.12}
{ Clear Body property in preparation for large attachment }
procedure TIpMimeEntity.ClearBodyLargeAttach(const AttachmentSize : Longint);
var
  FileName : string;
  Strm : TIpMemMapStream;
begin
  FBody.FreeStream;
  FileName := GetTemporaryFile(GetTemporaryPath);
  if FileExistsUTF8(FileName) then
    DeleteFileUTF8(FileName);
  Strm := TIpMemMapStream.Create(FileName, False, True);
  Strm.Size := Trunc(AttachmentSize * 1.3695);
  Strm.Open;
  FBody.Stream := Strm;
end;
{End !!.12}

{ Clear all properties }
procedure TIpMimeEntity.Clear;
begin
  ClearBody;
  FMimeParts.Clear;
  FBoundary := '';
  FCharacterSet := '';
  FContentDescription := '';
  FContentDispositionType := '';
  FContentID := '';
  FContentSubtype := '';
  FContentType := '';
  FContentTransferEncoding := emUnknown;
  FFileName := '';
  FIsMime := False;
  FIsMultipart := False;
  FMimeVersion := '';
  FEntityName := '';
  FRelatedType := '';                                                {!!.02}
  FRelatedSubtype := '';                                             {!!.02}
  FRelatedStart := '';                                               {!!.02}
  FRelatedStartInfo := '';                                           {!!.02}
end;

{ Build Mime (and nested Mime) block(s) from incoming text stream }
function TIpMimeEntity.DecodeEntity(InStream : TIpAnsiTextStream) : string;
var
  Blk : TIpMimeEntity;
  RawHeaders : TStringList;
  Decoded : Boolean;                                                   {!!.12}
  i,                                                                   {!!.13}
  LeadingBlankLines : Integer;                                         {!!.13}
begin
  Decoded := False;                                                    {!!.12}
  LeadingBlankLines := 0;                                              {!!.13}
  { skip blank lines in front of mime headers or body }
  Result := InStream.ReadLine;
  while (Result = '') and not InStream.AtEndOfStream do begin
    inc(LeadingBlankLines);
    Result := InStream.ReadLine;
  end;

  { decode mime headers if any }
{Begin !!.15}
  if (StrLIComp(PChar(strContent), PChar(Result), Length(strContent)) = 0) or
     (StrLIComp(PChar(strMimeVersion), PChar(Result),
                Length(strMimeVersion)) = 0) then begin
{End !!.15}
    RawHeaders := TStringList.Create;
    try
      repeat
        RawHeaders.Add(Result);
        Result := InStream.ReadLine;
      until (Result = '') or (InStream.AtEndOfStream);
      DecodeMimeHeaders(RawHeaders);
    finally
      RawHeaders.Free;
    end;
    Result := InStream.ReadLine;
    { skip blank lines between mime headers and mime body }
    while (Result = '') and not InStream.AtEndOfStream do
      Result := InStream.ReadLine;
  end;

  { decode body - main loop }
{Begin !!.15}
  if (FParentBoundary <> '') and
     (Result = '--' + FParentBoundary) then
    { The body of this entity is empty & we are now positioned at the boundary
      marker for the next entity. }
    Decoded := True
  else
{End !!.15}
  while not (((FParentBoundary <> '') and                              {!!.12}
              (Result = '--' + FParentBoundary)                        {!!.12}
             ) or InStream.AtEndOfStream) do begin                     {!!.12}
    Decoded := True;
    { check for ending boundary - in which case were done }
    if (FParentBoundary <> '') then
      if Pos('--' + FParentBoundary + '--', Result) = 1 {> 0} then begin
        Result := InStream.ReadLine;
        Exit;
      end;

    { decode any nested mime parts - recursively }
    if IsMultiPart and (Boundary <> '') and                            {!!.03}
      (Pos('--' + Boundary, Result) = 1)  then begin
      Blk := TIpMimeEntity.Create(Self);
      Result := Blk.DecodeEntity(Instream);
      FMimeParts.Add(Blk);
    end else begin
      { read raw text line into body }
      for i := 1 to LeadingBlankLines do                               {!!.13}
        Body.WriteLine('');                                            {!!.13}
      Body.WriteLine(Result);
      Result := InStream.ReadLine;
    end;
    if InStream.AtEndOfStream then break;                              {!!.12}
    LeadingBlankLines := 0;                                            {!!.13}
  end;
{Begin !!.12}
  { If did not find a MIME entity then assume the body is text &
    read it into the Body property. }
  if not Decoded then
    ReadBody(InStream, Result)
  else if (not (Pos('--' + FParentBoundary, Result) = 1)) then
    { If the last line is not a MIME separator then add the last line
      to the Body. }
    Body.WriteLine(Result);
{End !!.12}
end;

{!!.01}
{ Build Mime block as subpart from incoming text stream }
function TIpMimeEntity.DecodeEntityAsAttachment(InStream : TIpAnsiTextStream) : string;
var
  Blk : TIpMimeEntity;
begin
  Blk := TIpMimeEntity.Create(Self);
  Blk.ContentType := FContentType;
  Blk.ContentSubtype := FContentSubtype;
  Blk.ContentDispositionType := FContentDispositionType;
  Blk.ContentDescription := FContentDescription;
  Blk.ContentTransferEncoding := FContentTransferEncoding;
  Blk.CharacterSet := FCharacterSet;
  Blk.CreationDate := FCreationDate;
  Blk.FileName := FFileName;
  Blk.EntityName := FEntityName;
  Blk.FIsMime := True;
  Blk.FIsMultipart := False;
  Blk.ModificationDate := FModificationDate;
  Blk.MimeVersion := FMimeVersion;
  Blk.OriginalSize := FOriginalSize;
  Blk.ReadDate := FReadDate;

  Result := Blk.DecodeEntity(Instream);
  FMimeParts.Add(Blk);
  Body.Position := 0;
end;

{ Decode Content-Disposition header field and sub-fields }
procedure TIpMimeEntity.DecodeContentDisposition(const aDisp : string);
var
  RawParams : TStringList;
  S : string;
begin
  { split up parameters }
  RawParams := TStringList.Create;
  try
    Parse(aDisp, ';', RawParams);

    { decode disposition type and parameters }
    if (RawParams.Count > 0) then begin
      FContentDispositionType := RawParams[0];
      if (RawParams.Count > 1) then begin
        DecodeSingleParameter(strFileName, RawParams, FFileName);
        DecodeSingleParameter(strCreationDate, RawParams, FCreationDate);
        DecodeSingleParameter(strModificationDate, RawParams, FModificationDate);
        DecodeSingleParameter(strReadDate, RawParams, FReadDate);
        DecodeSingleParameter(strSize, RawParams, S);
        FOriginalSize := StrToIntDef(S, 0);
      end;
    end else
      FContentDispositionType := '';
  finally
    RawParams.Free;
  end;
end;

{ Decode Content-Type header field and sub-fields }
procedure TIpMimeEntity.DecodeContentType(const aType : string);
var
  RawParams : TStringList;
  S : string;
  i : Integer;
begin
  { split up parameters }
  RawParams := TStringList.Create;
  try
    Parse(aType, ';', RawParams);

    { decode type and subtype }
    FContentType := '';
    FContentSubType := '';
    if (RawParams.Count > 0) then begin
      S := RawParams[0];
      i := IpUtils.CharPos('/', S);
      if (i > 0) then begin
        FContentType := Copy(S, 1, i - 1);
        FContentSubType := Copy(S, i + 1, Length(S));
      end else
        FContentType := S;
    end;
    FIsMultipart := StrIComp(PChar(FContentType), PChar(strMultipart)) = 0;

    { decode the parameters }
    DecodeSingleParameter(strName, RawParams, FEntityName);
    DecodeSingleParameter(strBoundary, RawParams, FBoundary);
    DecodeSingleParameter(strCharSet, RawParams, FCharacterSet);

    {!!.02}
    { decode multipart/related parameters }
    DecodeSingleParameter(strType, RawParams, S);
    if (S <> '') then begin
      i := IpUtils.CharPos('/', S);
      if (i > 0) then begin
        FRelatedType := Copy(S, 1, i - 1);
        FRelatedSubType := Copy(S, i + 1, Length(S));
      end else
        FRelatedType := S;
      DecodeSingleParameter(strStart, RawParams, FRelatedStart);
      DecodeSingleParameter(strStartInfo, RawParams, FRelatedStartInfo);
    end;
    {!!.02}

  finally
    RawParams.Free;
  end;
end;

{ Decode Content-TranferEncoding header field }
function TIpMimeEntity.DecodeContentTransferEncoding(const aEncoding : string) :
  TIpMimeEncodingMethod;
begin
  if (UpperCase(aEncoding) = UpperCase(str7Bit)) then
    Result := em7bit
  else if (UpperCase(aEncoding) = UpperCase(str8Bit)) then
    Result := em8bit
  else if (UpperCase(aEncoding) = UpperCase(strBase64)) then
    Result := emBase64
  else if (UpperCase(aEncoding) = UpperCase(strBinary)) then
    Result := emBinary
  else if (UpperCase(aEncoding) = UpperCase(strBinHex)) then
    Result := emBinHex
  else if (UpperCase(aEncoding) = UpperCase(strQuoted)) then
    Result := emQuoted
  else if (UpperCase(aEncoding) = UpperCase(strUUEncode)) then
    Result := emUUEncode
  else
    Result := emUnknown;
end;


{ Decode Mime headers from raw header list }
procedure TIpMimeEntity.DecodeMimeHeaders(RawHeaders : TStringList);
var
  S : string;
begin
  { decode content type header }
  DecodeSingleHeader(strContentType, RawHeaders, S);
  if (S <> '') then begin
    FIsMime := True;
    DecodeContentType(S);
    if FIsMultipart and (FBoundary = '') then
      raise EIpBaseException.Create(SNoBoundary);
  end else begin
    FIsMime := False;
    Exit;
  end;

  { decode the others }
  DecodeSingleHeader(strMIMEVersion, RawHeaders, FMimeVersion);
  DecodeSingleHeader(strContentTransferEncoding, RawHeaders, S);
  FContentTransferEncoding := DecodeContentTransferEncoding(S);
  DecodeSingleHeader(strContentDescription, RawHeaders, FContentDescription);
  DecodeSingleHeader(strContentID, RawHeaders, FContentID);
  DecodeSingleHeader(strContentDisposition, RawHeaders, S);
  if (S <> '') then
    DecodeContentDisposition(S);
  if (FContentDispositionType = strAttachment) then                    {!!.12}
    Inc (FParent.FAttachmentCount);                                    {!!.12}{!!.15}
end;

{ Compute attachment coding progress and fire OnCodingProgress event }
procedure TIpMimeEntity.DoOnCodingProgress(Count, TotalSize : Longint;
                                          var Abort : Boolean);
  { IMPORTANT: The progress event must only be fired by the root parent }
begin
  if (Parent = nil) or (Parent = Self) then begin
    FProgress := ((Count*100) div TotalSize);
    if (FProgress > 100) then
      FProgress := 100;
    if (FProgress div 10) = 0 then
      PrevProgress := 0;

    { report progress in 10% increments }
    if ((FProgress div 10) > (PrevProgress div 10)) then begin
      PrevProgress := FProgress;
      if Assigned(FOnCodingProgress) then
        FOnCodingProgress(Self, FProgress, Abort);
    end;
  end else
    Parent.DoOnCodingProgress(Count, TotalSize, Abort);
end;

{ Generate Mime message stream from properties (and nested Mime blocks) }
function TIpMimeEntity.EncodeEntity(OutStream : TIpAnsiTextStream) : string;
var
  i : Integer;
  S : string;
  RawHeaders : TStringList;
  Ch : AnsiChar;
begin
  Result := FParentBoundary;

  { write out mime headers }
  if (Result <> '') then begin
    OutStream.WriteLine('--' + Result);
    RawHeaders := TStringList.Create;
    try
      EncodeMimeHeaders(RawHeaders);
      if (RawHeaders.Count > 0) then
        for i := 0 to Pred(RawHeaders.Count) do
          if (RawHeaders[i] <> '') then
            OutStream.WriteLine(RawHeaders[i]);
      OutStream.WriteLine('');
    finally
      RawHeaders.Free;
    end;
  end;

  // flush to update underlaying memory streams
  Body.Flush;
  { write out mime body }
  if (Body.FastSize > 0) then
  begin
    // presize stream for more speed
    OutStream.Stream.Size := OutStream.Stream.Size + Body.FastSize;
    // use optimal method depending on the source stream to copy the stream
    if Body.Stream is TIpMemMapStream then
      OutStream.Write((Body.Stream as TIpMemMapStream).Memory^, Body.FastSize)
    else   
      if Body.Stream is TMemoryStream then
        OutStream.Write((Body.Stream as TMemoryStream).Memory^, Body.Stream.Size)
      else 
        OutStream.CopyFrom(Body, 0); // copy the entire stream from the beginning

    { make sure the body is properly terminated }                    {!!.01}
    OutStream.Position := OutStream.Size - 1;                        {!!.01}
    TIpBufferedStream(OutStream).ReadChar(Ch);                       {!!.01}
    if ((Ch <> #13) and (Ch <> #10)) then                            {!!.01}
      OutStream.WriteLine('');                                       {!!.01}
  end;

  { encode nested mime parts - recursively }
  if (FMimeParts.Count > 0) then begin
    for i := 0 to Pred(FMimeParts.Count) do
      S := FMimeParts[i].EncodeEntity(OutStream);
    OutStream.WriteLine('--' + S + '--');
  end;
end;

{Begin !!.14}
function TIpMimeEntity.ContainsSpecialChars(const Value : string) : Boolean;
var
  Inx : Integer;
begin
  Result := False;
  for Inx := 1 to Length(Value) do
    if (Ord(Value[Inx]) <= 32) or
       (Value[Inx] in ['(', ')', '<', '>', '@',
                       ',', ';', ':', '\', '"',
                       '/', '[', ']', '?', '=']) then begin
      Result := True;
      Break;
    end; { if }
end;
{End !!.14}

{ Generate Content-Disposition header into raw header list }
procedure TIpMimeEntity.EncodeContentDisposition(RawHeaders : TStringList);
var
  Params : TStringList;
begin
  if (FContentDispositionType = '') then
    Exit;

  Params := TStringList.Create;
  try
    Params.Add(FContentDispositionType);
{Begin !!.14}
    if (FFileName <> '') then begin
      { If the filename contains spaces, control characters, or any of the
        special characters identified in RFC 1521 then wrap the filename in
        quotes.

        Assumption: FFileName length is <= 78 characters. Future enhancement
        is to support RFC 2184. }
      if ContainsSpecialChars(FFileName) then
        Params.Add(strFileName + '"' + FFileName + '"')
      else
        Params.Add(strFileName + FFileName);
    end;  { if }
{End !!.14}
    if (FCreationDate <> '') then
      Params.Add(strCreationDate + FCreationDate);
    if (FModificationDate <> '') then
      Params.Add(strModificationDate + FModificationDate);
    if (FReadDate <> '') then
      Params.Add(strReadDate + FReadDate);
    if (FOriginalSize > 0) then
      Params.Add(strSize + IntToStr(FOriginalSize));
    EncodeListHeader(strContentDisposition, RawHeaders, Params, ';', False);
  finally
    Params.Free;
  end;
end;

{ Generate Content-Type header into raw header list }
procedure TIpMimeEntity.EncodeContentType(RawHeaders : TStringList);
var
  S : string;
  Params : TStringList;
begin
  if (FContentType = '') then
    Exit;

  Params := TStringList.Create;
  try
    S := FContentType;
    if (FContentSubType <> '') then
      S := S + '/' + FContentSubType;
    Params.Add(S);
    if IsMultipart then
      Params.Add(strBoundary + '"' + FBoundary + '"');
    if (FEntityName <> '') then
      Params.Add(strName + '"' + FEntityName + '"');
    if (FCharacterSet <> '') then
      Params.Add(strCharSet + FCharacterSet); {no quotes}

    {!!.02}
    { encode multipart/related parameters }
    if (FRelatedType <> '') then begin
      if (FRelatedSubtype <> '') then
        Params.Add(strType + '"' + FRelatedType + '/' + FRelatedSubtype + '"')
      else
        Params.Add(strType + '"' + FRelatedType + '"');
      if (FRelatedStart <> '') then
        Params.Add(strStart + '"' + FRelatedStart + '"');
      if (FRelatedStartInfo <> '') then
        Params.Add(strStartInfo + '"' + FRelatedStartInfo + '"');
    end;
    {!!.02}

    EncodeListHeader(strContentType, RawHeaders, Params, ';', False);
  finally
    Params.Free;
  end;
end;

{ Generate Content-TranferEncoding header into raw header list }
procedure TIpMimeEntity.EncodeContentTransferEncoding(RawHeaders : TStringList);
begin
  case FContentTransferEncoding of
    em7bit     : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, str7Bit);
    em8bit     : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, str8Bit);
    emBase64   : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, strBase64);
    emBinary   : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, strBinary);
    emBinHex   : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, strBinHex);
    emQuoted   : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, strQuoted);
    emUUEncode : EncodeSingleHeader(strContentTransferEncoding, RawHeaders, strUUEncode);
  end;
end;

{ Generate Mime headers into raw header list }
procedure TIpMimeEntity.EncodeMimeHeaders(RawHeaders : TStringList);
begin
  if (FContentType <> '') then begin
    EncodeSingleHeader(strMimeVersion, RawHeaders, FMimeVersion);
    EncodeContentType(RawHeaders);
    EncodeSingleHeader(strContentDescription, RawHeaders, FContentDescription);
    EncodeSingleHeader(strContentID, RawHeaders, FContentID);
    EncodeContentTransferEncoding(RawHeaders);
    EncodeContentDisposition(RawHeaders);
  end;
end;

{ Encode Mime body from TStream - file name is optional }
procedure TIpMimeEntity.EncodeBodyStream(InStream : TStream; const aFileName : string);
{Begin !!.12}
var
  LargeAttachment : Boolean;
    { Large attachments are handled with memory map streams in order to avoid
      whacko memory issues with TMemoryStream. }
begin
  if (Instream.Size > 0) then begin
    LargeAttachment := (InStream.Size > IpLgAttachSizeBoundry);
    if LargeAttachment then
      ClearBodyLargeAttach(InStream.Size)
    else
    begin
      ClearBody;
      // presize stream for more speed
      FBody.Stream.Size := Trunc(InStream.Size * 1.3695);
    end;
{End !!.12}
    case FContentTransferEncoding of
      em7Bit     : Encode8Bit(InStream);
      em8Bit     : Encode8Bit(InStream);
      emBase64   : EncodeBase64(InStream);
      emBinary   : Encode8Bit(InStream);
      emBinHex   : EncodeBinHex(InStream, aFileName);
      emQuoted   : EncodeQuoted(InStream);
      emUUEncode : EncodeUUEncode(InStream, aFileName);
      emUnknown  : Encode8Bit(InStream);
    end;
  {Begin !!.12}
    FBody.Flush;
    if LargeAttachment then
      { This is a large attachment that was written to a memory map stream.
        Memory map streams are usually created larger than necessary so shrink
        it down to the correct size. }
      TIpMemMapStream(FBody.Stream).Size := TIpMemMapStream(FBody.Stream).DataSize;
  {End !!.12}
  end;
  FOriginalSize := InStream.Size;
  FFileName := ExtractFileName(aFileName);
end;

{ Encode Mime body from TStrings - file name is optional }
procedure TIpMimeEntity.EncodeBodyStrings(InStrings : TStrings; const aFileName : string);
var
  MS : TMemoryStream;
begin
  if (InStrings.Count > 0) then begin
    MS := TMemoryStream.Create;
    try
      InStrings.SaveToStream(MS);
      MS.Position := 0;                                              {!!.03}
      FOriginalSize := MS.Size;
      FFileName := ExtractFileName(aFileName);
      EncodeBodyStream(MS, aFileName);
    finally
      MS.Free;
    end;
  end;
end;

{ Encode Mime body from file }
procedure TIpMimeEntity.EncodeBodyFile(const InFile : string);
var
  FS : TIpMemMapStream;                                                {!!.12}
  i : Integer;
  aExt, aTyp, aSub : string;
  aEnc : TIpMimeEncodingMethod;
begin
  { If content-type, has not been defined for this entity,    }
  { default values for that file extension  will be used.     }
  { These values are defined in the include file, IPDEFCT.INC }
  aTyp := strApplication;
  aSub := strOctetStream;
  aEnc := emBase64;
  aExt := ExtractFileExt(InFile);
  for i := 0 to High(DefExtensions) do
    if (aExt = DefExtensions[i]) then begin
      aTyp := DefContent[i].Typ;
      aSub := DefContent[i].Sub;
      aEnc := DefContent[i].Enc;
      Break;
    end;
  if (FContentType = '') then begin
    FContentType := aTyp;
    FContentSubtype := aSub;
    FContentTransferEncoding := aEnc;
  end;
  if (FContentTransferEncoding = emUnknown) then
    FContentTransferEncoding := aEnc;

  FS := TIpMemMapStream.Create(InFile, True, False);                   {!!.12}
  try
    FS.Open;                                                           {!!.12}
    FOriginalSize := FS.Size;
    FFileName := ExtractFileName(InFile);
    EncodeBodyStream(FS, FFileName);
  finally
    FS.Free;
  end;
end;

{ Decode encoded Mime block body to TStream }
procedure TIpMimeEntity.ExtractBodyStream(OutStream : TStream);
var
  MS : TMemoryStream;
begin
  if (FBody.Size > 0) then begin
   { We want to append the decoded data to the end of OutStream, }
   { so a local memory stream is used since OutStream may be a   }
   { TIpAnsiTextStream, in which case the decoding algorithms    }
   { will overwrite its existing contents.                       }
    MS := TMemoryStream.Create;
    try
      case FContentTransferEncoding of
        em7Bit     : Decode8Bit(MS);
        em8Bit     : Decode8Bit(MS);
        emBase64   : DecodeBase64(MS);
        emBinary   : OutStream.CopyFrom(FBody, FBody.Size);            {!!.14}
        emBinHex   : DecodeBinHex(MS);
        emQuoted   : DecodeQuoted(MS);
        emUUEncode : DecodeUUEncode(MS);
        emUnknown  : Decode8Bit(MS);
      end;
      OutStream.CopyFrom(MS, 0);
    finally
      MS.Free;
    end;
  end;
end;

{ Decode encoded Mime block body to TStrings }
procedure TIpMimeEntity.ExtractBodyStrings(OutStrings : TStrings);
var
  MS : TMemoryStream;
begin
  if (FBody.Size > 0) then begin
    MS := TMemoryStream.Create;
    try
      ExtractBodyStream(MS);
      MS.Position := 0;
      OutStrings.LoadFromStream(MS);
    finally
      MS.Free;
    end;
  end;
end;

{ Decode encoded Mime block body to file }
procedure TIpMimeEntity.ExtractBodyFile(const OutFile : string);
var
  FS : TFileStream;
begin
  if (FBody.Size > 0) then begin
    FS := TFileStream.Create(UTF8ToSys(OutFile), fmCreate);
    try
      ExtractBodyStream(FS);
    finally
      FS.Free;
    end;
  end;
end;

{ Access/create specified MIME part }
function TIpMimeEntity.GetMimePart(const aType, aSubType, aContentID : string;
                                       CanCreate : Boolean) : TIpMimeEntity;
var
  i : Integer;
begin
  Result := nil;
  if (MimeParts.Count > 0) then
    for i := 0 to Pred(MimeParts.Count) do
      { ContentID is primary search key }
      if (aContentID <> '') then begin
        if (MimeParts[i].ContentID = aContentID) then begin
          Result := MimeParts[i];
          Break;
        end;
      end else begin
        if (MimeParts[i].ContentType = aType) and
           (MimeParts[i].ContentSubtype = aSubType) then begin
          Result := MimeParts[i];
          Break;
        end;
      end;

  if Assigned(Result) then
    Result.Body.Position := 0
  else if CanCreate then begin
    Result := NewMimePart;
    Result.ContentType := aType;
    Result.ContentSubtype := aSubtype;
    Result.ContentID := aContentID;
  end;
end;

{!!.02}
{ Search all nested levels for specified MIME part }
function TIpMimeEntity.FindNestedMimePart(const aType, aSubType, aContentID : string) : TIpMimeEntity;
var
  i : Integer;
  Blk : TIpMimeEntity;
begin
  Result := nil;
  if (MimeParts.Count > 0) then
    for i := 0 to Pred(MimeParts.Count) do begin
      { ContentID is primary search key }
      if (aContentID <> '') and                                          {!!.12}
         (IsSameString (MimeParts[i].ContentID,                          {!!.12}
                        aContentID, False)) then begin                   {!!.12}
        Result := MimeParts[i];
        Break;
      end else if (IsSameString (MimeParts[i].ContentType,               {!!.12}
                                 aType, False)) and                      {!!.12}
                  (IsSameString (MimeParts[i].ContentSubtype,            {!!.12}
                                 aSubType, False)) then begin            {!!.12}
        Result := MimeParts[i];
        Break;
      end else begin
        Blk := MimeParts[i];
        Result := Blk.FindNestedMimePart(aType, aSubType, aContentID);
        if Assigned(Result) then
          Break;
      end;
    end;
  if Assigned(Result) then
    Result.Body.Position := 0;
end;

{ Create nested Mime block and add to list }
function TIpMimeEntity.NewMimePart : TIpMimeEntity;
begin
  {parent Entity is now multipart}
  FIsMime := True;
  FIsMultipart := True;
  FContentType := strMultipart;
  if (FBoundary = '') then
    FBoundary := GenerateBoundary;

  Result := TIpMimeEntity.Create(Self);
  FMimeParts.Add(Result);
end;

{ Copy Instream to OutStream as is - no decoding }
procedure TIpMimeEntity.Decode8Bit(OutStream : TStream);
var
  FS : TIpAnsiTextStream;
  Abort : Boolean;
begin
  Abort := False;
  FS := TIpAnsiTextStream.Create(OutStream);
  try
    FBody.Position := 0;
    while (FBody.Position < FBody.Size) and not Abort do begin
      FS.WriteLine(FBody.ReadLine);
      DoOnCodingProgress(OutStream.Position, FBody.Size, Abort);
    end;
  finally
    FS.Free;
  end;
end;

{ Decode InStream to OutStream - Base64 }
procedure TIpMimeEntity.DecodeBase64(OutStream : TStream);
  { rewritten }                                                      {!!.12}
var
  I : Integer;                                                         {!!.16}
  C : Char;
  InBuf  : array[0..3] of Char;
  OutBuf : array[0..2] of Byte;
  Done  : Boolean;
  Abort : Boolean;
  BufStream : TIpBufferedStream;
begin
  BufStream := (FBody as TIpBufferedStream);
  BufStream.Position := 0;
  Done := False;
  Abort := False;

  while not (Done or Abort) do begin
    { read in the next 4 valid Base64 characters }
    I := 0;
    InBuf := '====';                                                   {!!.15}
    while (I < 4) do begin
      if not BufStream.ReadChar(C) then begin
        Done := True;
        Break;
      end;

      { skip bad characters }
      if (Low(IpD64Table) <= C) and (C <= High(IpD64Table)) then
        if (IpD64Table[C] <> $7F) then begin
          InBuf[I] := C;
          Inc(I);
        end;
    end;

    { Decode 4 characters to 3 bytes }
    I := 0;
    OutBuf[0] := ((IpD64Table[InBuf[0]] shl 2) or (IpD64Table[InBuf[1]] shr 4));
    Inc(I);
    if InBuf[2] <> '=' then begin
      OutBuf[1] := ((IpD64Table[InBuf[1]] shl 4) or (IpD64Table[InBuf[2]] shr 2));
      Inc(I);
      if InBuf[3] <> '=' then begin
        OutBuf[2] := ((IpD64Table[InBuf[2]] shl 6) or IpD64Table[InBuf[3]]);
        Inc(I);
      end else
        Done := True;
    end else
        Done := True;
    OutStream.Write(OutBuf, I);
    DoOnCodingProgress(OutStream.Position, BufStream.FastSize, Abort); {!!.16}
  end;
end;

{ Decode InStream to OutStream - BinHex }
procedure TIpMimeEntity.DecodeBinHex(OutStream : TStream);
var
  InBuf : array[1..4] of Byte;
  OutBuf : array[1..3] of Byte;
  i : Byte;
  btThis, btLast, btNext : Byte;
  ch : AnsiChar;
  // headerlength is encoded as byte, HeaderFileName can only 256 bytes long
  HeaderFileName : Array [0..MaxByte] of Byte;                   {!!.12}{!!.16}
  HeaderLength : byte;                                                  {!!.12}
  CRC : Word;
  DataOffset, DataEnd, HeaderEnd : Longint;
  WS1, WS2 : TMemoryStream;
  Header : BinHexHeader;
  Abort : Boolean;
  BufStream : TIpBufferedStream;

  function NextChar : AnsiChar;
    {- skip past any CRLF's and return the next message stream char }
  var
    c : AnsiChar;
  begin
    c := #0;
    repeat
      BufStream.ReadChar(c);
    until ((c <> #13) and (c <> #10)) or (BufStream.Position = BufStream.Size);
    Result := c;
  end;

  function ValidChar(ch : AnsiChar) : Boolean;
    {- test if ch is a valid BinHex encoded char }
  var
    b : Byte;
  begin
    Result := False;
    b := Ord(ch);
    if (b > 32) and (b < 115) then
      if IpHexBinTable[b] <> $0FF then
        Result := True;
  end;

begin
  Abort := False;
  FBody.Position := 0;
  if Pos('(This file must be converted with BinHex', FBody.ReadLine) = 0 then
    raise EIpBaseException.Create(SBinHexBadFormat);
  if (NextChar <> ':') then
    raise EIpBaseException.Create(SBinHexColonExpected);

  { decode attachment into working stream }
  BufStream := (FBody as TIpBufferedStream);
  WS1 := TMemoryStream.Create;
  try
    i := 0;
    ch := NextChar;
    while (ch <> ':') and (BufStream.Position < BufStream.Size) and not Abort do begin
      if not ValidChar(ch) then
        raise EIpBaseException.Create(SBinHexBadChar);
      Inc(i);
      InBuf[i] := IpHexBinTable[Ord(ch)];
      { decode 4 characters into 3 bytes }
      if (i = 4) then begin
        i := 0;
        { 1st :    upper 6          lower 2 }
        OutBuf[1] := (InBuf[1] shl 2) or ((InBuf[2] shr 4) and $03);
        { 2nd :    upper 4          lower 4 }
        OutBuf[2] := (InBuf[2] shl 4) or ((InBuf[3] shr 2) and $0F);
        { 3rd :    upper 2          lower 6 }
        OutBuf[3] := (InBuf[3] shl 6) or (InBuf[4] and $03F);
        WS1.Write(OutBuf, SizeOf(OutBuf));
      end;
      ch := NextChar;
    end;

    { handle odd characters }
    if (i > 0) then begin
      if (i = 1) then
        raise EIpBaseException.Create(SBinHexOddChar);
      OutBuf[1] := (InBuf[1] shl 2) or ((InBuf[2] shr 4) and $03);
      if (i = 2) then
        WS1.Write(OutBuf, 1)
      else begin
        OutBuf[2] := (InBuf[2] shl 4) or ((InBuf[3] shr 2) and $0F);
        WS1.Write(OutBuf, 2);
      end;
      DoOnCodingProgress(BufStream.Position, BufStream.Size, Abort);
    end;
    if Abort then
      Exit;

    { should be the end of file marker }
    if (ch <> ':') then
      raise EIpBaseException.Create(SBinHexColonExpected);

    { expand RLE sequences }
    WS2 := TMemoryStream.Create;
    try
      WS1.Position := 0;
      btThis := 0;
      while (WS1.Position < WS1.Size) and not Abort do begin
        btLast := btThis;
        WS1.Read(btThis, 1);
        if (btThis <> RLEChar) then
          WS2.Write(btThis, 1)
        else begin
          WS1.Read(btNext, 1);
          if (btNext = 0) then
            WS2.Write(btThis, 1)
          else begin
            btThis := btLast;
            for i := 1 to (btNext - 1) do
              WS2.Write(btThis, 1);
          end;
        end;
        DoOnCodingProgress(WS1.Position, WS1.Size, Abort);
      end;
      if Abort then
        WS2.Free;

      { strip off header }
      FillChar (HeaderFileName, SizeOf (HeaderFileName), $00);           {!!.12}
      FillChar(Header, SizeOf(Header), #0);
      WS2.Position := 0;
      WS2.Read(HeaderLength, SizeOf (Byte));                             {!!.12}
      WS2.Read(HeaderFileName, HeaderLength);                            {!!.12}
      WS2.Read(Header, SizeOf(Header));

      { check header CRC }
      HeaderEnd := WS2.Position;
      WS2.Read(CRC, 2);
      DataOffset := WS2.Position;
      if (CRC <> BinHexCRC(WS2, 0, HeaderEnd)) then
        raise EIpBaseException.Create(SBinHexBadHeaderCRC);
      DataEnd := DataOffset + htonl(Header.DFLong);
      if (DataEnd > WS2.Size) then
        raise EIpBaseException.Create(SBinHexLengthErr);
      if (htonl(Header.RFLong) > 0) then
        raise EIpBaseException.Create(SBinHexResourceForkErr);

      { check data fork CRC - follows data fork }
      WS2.Position := DataEnd;
      WS2.Read(CRC, 2);
      if (CRC <> BinHexCRC(WS2, DataOffset, DataEnd)) then
        raise EIpBaseException.Create(SBinHexBadDataCRC);

      { copy data fork to OutStream }
      WS2.Position := DataOffset;
      OutStream.CopyFrom(WS2, DataEnd - DataOffset);
    finally
      WS2.Free;
    end;
  finally
    WS1.Free;
  end;
end;

{ Decode InStream to OutStream - QuotedPrintable }
procedure TIpMimeEntity.DecodeQuoted(OutStream : TStream);
var
  O, Count, WS : Byte;                                                   {!!.12}
  I : integer;                                                           {!!.12}
  InBuf  : array[0..pred (MaxLine)] of Byte;                             {!!.15}
  OutBuf : array[0..pred (MaxLine)] of Byte;                             {!!.15}
  Decoding : Boolean;
  Keeper : Boolean;
  Abort : Boolean;
  BufStream : TIpBufferedStream;
begin
  Abort := False;
  FBody.Position := 0;
  BufStream := FBody as TIpBufferedStream;
  FillChar(InBuf, SizeOf(InBuf), #0);
  WS := $FF;
  Decoding := True;
  Keeper := False;

  { Skip any CR/LF's to get to the encoded stuff }
  while True do begin
    if not BufStream.ReadChar(Char(InBuf[0])) then
      Exit;
    if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
      Keeper := True;
      Break;
    end;
  end;

  while Decoding and not Abort do begin
    { Initialize }
    if Keeper then begin
      I := 1;
      Keeper := False;
    end else begin
      I := 0;
    end;
    O := 0;

    { Read in one line at a time - skipping over bad characters }
    while True do begin
      if (I > High(InBuf)) then                                        {!!.01}
        raise EIpBaseException.Create(SLineLengthErr);                 {!!.01}
      if not BufStream.ReadChar(Char(InBuf[I])) then
        Break;
      case InBuf[I] of
        $0A : Continue;
        $0D : begin
                Inc(I);
                Break;
              end;
       { Test for potential end of data }
       { '--' is probably the next Mime boundary }
       { $2D : if (I = 1) and (InBuf[0] = $2D) then Exit;}             {!!.03}
      end;
      Inc(I);
    end;

    if I = 0 then Exit;
    Count := I;
    I := 0;

    { Decode data to output stream }
    while I < Count do begin
      case InBuf[I] of
        9       : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        13      : if WS = $FF then begin
                    OutBuf[O] := 13;
                    OutBuf[O+1] := 10;
                    Inc(O, 2);
                    Inc(I);
                  end else begin
                    OutBuf[WS] := 13;
                    OutBuf[WS+1] := 10;
                    O := WS+2;
                    Inc(I);
                  end;
        32      : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        33..60  : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        61      : begin
                    WS := $FF;
                    if I+2 >= Count then Break;
                    case InBuf[I+1] of
                      48 : OutBuf[O] := 0;    {0}
                      49 : OutBuf[O] := 16;   {1}
                      50 : OutBuf[O] := 32;   {2}
                      51 : OutBuf[O] := 48;   {3}
                      52 : OutBuf[O] := 64;   {4}
                      53 : OutBuf[O] := 80;   {5}
                      54 : OutBuf[O] := 96;   {6}
                      55 : OutBuf[O] := 112;  {7}
                      56 : OutBuf[O] := 128;  {8}
                      57 : OutBuf[O] := 144;  {9}
                      65 : OutBuf[O] := 160;  {A}
                      66 : OutBuf[O] := 176;  {B}
                      67 : OutBuf[O] := 192;  {C}
                      68 : OutBuf[O] := 208;  {D}
                      69 : OutBuf[O] := 224;  {E}
                      70 : OutBuf[O] := 240;  {F}
                      97 : OutBuf[O] := 160;  {a}
                      98 : OutBuf[O] := 176;  {b}
                      99 : OutBuf[O] := 192;  {c}
                     100 : OutBuf[O] := 208;  {d}
                     101 : OutBuf[O] := 224;  {e}
                     102 : OutBuf[O] := 240;  {f}
                    end;
                    case InBuf[I+2] of
                      48 : ;                             {0}
                      49 : OutBuf[O] := OutBuf[O] + 1;   {1}
                      50 : OutBuf[O] := OutBuf[O] + 2;   {2}
                      51 : OutBuf[O] := OutBuf[O] + 3;   {3}
                      52 : OutBuf[O] := OutBuf[O] + 4;   {4}
                      53 : OutBuf[O] := OutBuf[O] + 5;   {5}
                      54 : OutBuf[O] := OutBuf[O] + 6;   {6}
                      55 : OutBuf[O] := OutBuf[O] + 7;   {7}
                      56 : OutBuf[O] := OutBuf[O] + 8;   {8}
                      57 : OutBuf[O] := OutBuf[O] + 9;   {9}
                      65 : OutBuf[O] := OutBuf[O] + 10;  {A}
                      66 : OutBuf[O] := OutBuf[O] + 11;  {B}
                      67 : OutBuf[O] := OutBuf[O] + 12;  {C}
                      68 : OutBuf[O] := OutBuf[O] + 13;  {D}
                      69 : OutBuf[O] := OutBuf[O] + 14;  {E}
                      70 : OutBuf[O] := OutBuf[O] + 15;  {F}
                      97 : OutBuf[O] := OutBuf[O] + 10;  {a}
                      98 : OutBuf[O] := OutBuf[O] + 11;  {b}
                      99 : OutBuf[O] := OutBuf[O] + 12;  {c}
                     100 : OutBuf[O] := OutBuf[O] + 13;  {d}
                     101 : OutBuf[O] := OutBuf[O] + 14;  {e}
                     102 : OutBuf[O] := OutBuf[O] + 15;  {f}
                    end;
                    Inc(I, 3);
                    Inc(O);
                  end;
        62..126 : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        else
          Inc(I);
      end;
    end;

    if O>0 then
      OutStream.Write(OutBuf, O)
    else
      Break;   { OutBuf is empty }
    DoOnCodingProgress(OutStream.Position, FBody.Size, Abort);
  end;
end;

{ Decode InStream to OutStream - UUEncode }
procedure TIpMimeEntity.DecodeUUEncode(OutStream : TStream);
var
  I, O, Len, Count : Byte;
  InBuf  : array[0..85] of Byte;
  OutBuf : array[0..65] of Byte;
  FirstLine : Boolean;
  Abort : Boolean;
  BufStream : TIpBufferedStream;
begin
  Abort := False;
  FBody.Position := 0;
  BufStream := FBody as TIpBufferedStream;
  FirstLine := True;
  while True and not Abort do begin
    { Initialize }
    I := 0;
    O := 0;

    { Skip any CR/LF's to get to the encoded stuff }
    while True do begin
      if not BufStream.ReadChar(Char(InBuf[0])) then
        Exit;
      if FirstLine then begin
        if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
          FirstLine := False;
          Break;
        end;
     end else begin
        if ((InBuf[0] = $0D) or (InBuf[0] = $0A)) then FirstLine := True;
      end;
    end;

    { We're done }
    if AnsiChar(InBuf[0]) = '`' then Exit;

    { Get count for this line }
    Len := (((InBuf[0] - $20) and $3F) * 4) div 3;
    if (((InBuf[0] - $20) and $3F) * 4) mod 3 <> 0 then
      Inc(Len);

    Count := FBody.Read(InBuf, Len);

    { Unexpected situation }
    if (Count <> Len) or (Count > 63) then
      raise EIpBaseException.Create(SUUEncodeCountErr);

    { Decode buffer }
    while (I < Count) do begin
      if ((Count - I) >= 4) then begin
        OutBuf[O] := (((InBuf[I] - $20) and $3F) shl 2) or
          (((InBuf[I+1] - $20) and $3F) shr 4);
        OutBuf[O+1] := (((InBuf[I+1] - $20) and $3F) shl 4) or
          (((InBuf[I+2] - $20) and $3F) shr 2);
        OutBuf[O+2] := (((InBuf[I+2] - $20) and $3F) shl 6) or
          (((InBuf[I+3] - $20) and $3F));
        Inc(O, 3);
      end else begin
        if (Count >= 2) then begin
          OutBuf[O] := (((InBuf[I] - $20) and $3F) shl 2) or
            (((InBuf[I+1] - $20) and $3F) shr 4);
          Inc(O);
        end;
        if (Count >= 3) then begin
          OutBuf[O+1] := (((InBuf[I+1] - $20) and $3F) shl 4) or
            (((InBuf[I+2] - $20) and $3F) shr 2);
          Inc(O);
        end;
      end;
      Inc(I, 4);
    end;
    OutStream.Write(OutBuf, O);
    DoOnCodingProgress(OutStream.Position, FBody.Size, Abort);
  end;
end;

{ Encode InStream to OutStream - as is, no encoding }
procedure TIpMimeEntity.Encode8Bit(InStream : TStream);
var
  FS : TIpAnsiTextStream;
  Abort : Boolean;
begin
  Abort := False;
  FS := TIpAnsiTextStream.Create(InStream);
  try
    while not (FS.AtEndOfStream or Abort) do begin
      FBody.WriteLine(FS.ReadLine);
      DoOnCodingProgress(FS.Position, FS.Size, Abort);
    end;
  finally
    FS.Free;
  end;
end;

{ Encode InStream to OutStream - Base64 }
procedure TIpMimeEntity.EncodeBase64(InStream : TStream);
begin
  OctetStreamToHextetStream(InStream, FBody, Ip64Table, '=', #0);
end;

{ Encode InStream to OutStream - BinHex }
procedure TIpMimeEntity.EncodeBinHex(InStream : TStream;
                                     const aFileName : string);
var
  HeaderFileName : string;                                              {!!.12}
  CRC : Word;
  DataOffset : DWord;
  PrevByte, CurrByte, i : Byte;
  Header : BinHexHeader;
  WS1, WS2 : TMemoryStream;
  Abort : Boolean;

begin
  Abort := False;
  WS1 := TMemoryStream.Create;
  try
    { start with file name }
    if (Length(aFileName) < MaxLine) then
      HeaderFileName := UpperCase(ExtractFileName(aFileName))
    else
      HeaderFileName := Copy(UpperCase(ExtractFileName(aFileName)), 1, MaxLine);
    WS1.Write(HeaderFileName, Length(HeaderFileName) + 1);

    { build rest of file header and header CRC and add to working stream }
    FillChar(Header, SizeOf(Header), #0);
    Move(BinHexFileType, Header.FileType, SizeOf(Header.FileType));
    Move(BinHexFileType, Header.Creator, SizeOf(Header.Creator));
    Header.DFLong := htonl(InStream.Size);
    Header.RFLong := 0;
    WS1.Write(Header, SizeOf(Header));
    CRC := BinHexCRC(WS1, 0, WS1.Size);
    WS1.Write(CRC, 2);

    { append data fork and data CRC to working stream }
    DataOffset := WS1.Position;
    InStream.Position := 0;
    WS1.CopyFrom(InStream, InStream.Size);
    CRC := BinHexCRC(WS1, DataOffset, WS1.Size);
    WS1.Write(CRC, 2);

    { tack on resource fork CRC - not used but still required }
    CRC := 0;
    WS1.Write(CRC, 2);

    { go back and compress RLE sequences }
    WS2 := TMemoryStream.Create;
    try
      WS1.Position := 0;
      CurrByte := 0;
      while (WS1.Position < WS1.Size) and not Abort do begin
        PrevByte := CurrByte;
        WS1.Read(CurrByte, 1);
        if (CurrByte <> PrevByte) then
          WS2.Write(CurrByte, 1)
        else begin
          i := 1;
          repeat
            i := i + WS1.Read(CurrByte, 1);
          until (CurrByte <> PrevByte) or (i = 255) or
                (WS1.Position = WS1.Size);
          if (i > 2) then begin
            WS2.Write(RLEChar, 1);
            WS2.Write(i, 1);
            WS2.Write(CurrByte, 1);
          end else begin
            WS2.Write(PrevByte, 1);
            WS2.Write(CurrByte, 1);
          end;
        end;
        DoOnCodingProgress(WS1.Position, WS1.Size, Abort);
      end;
      if Abort then
        Exit;

      { write out preamble }
      FBody.WriteLine('(This file must be converted with BinHex 4.0)');

      { Encode compressed stream and stream it out }
      WS2.Position := 0;
      OctetStreamToHextetStream(WS2, FBody, IpBinHexTable, #0, ':');
    finally
      WS2.Free;
    end;
  finally
    WS1.Free;
  end;
end;

{ Encode InStream to OutStream - QuotedPrintable }
procedure TIpMimeEntity.EncodeQuoted(InStream : TStream);
var
  O, W : Integer;
  WordBuf, OutBuf : array[0..80] of AnsiChar;
  CurChar : AnsiChar;
  Abort : Boolean;
  ByteStream : TIpByteStream;

  procedure SendLine;
  begin
    if (OutBuf[O-1] = #9) or (OutBuf[O-1] = #32) then begin
      OutBuf[O] := '=';
      Inc(O);
    end;
    FBody.WriteLineZ(OutBuf);
    FillChar(OutBuf, SizeOf(OutBuf), #0);
    O := 0;
  end;

  procedure AddWordToOutBuf;
  var
    J : Integer;
  begin
    if (O + W) > 74 then SendLine;
    for J := 0 to (W - 1) do begin
      OutBuf[O] := WordBuf[J];
      Inc(O);
    end;
    W := 0;
  end;

  procedure AddHexToWord(B : Byte);
  begin
    if W > 73 then AddWordToOutBuf;
    WordBuf[W] := '=';
    WordBuf[W+1] := HexDigits[B shr 4];
    WordBuf[W+2] := HexDigits[B and $F];
    Inc(W, 3)
  end;

begin
  Abort := False;
  O := 0;
  W := 0;
  FillChar(OutBuf, SizeOf(OutBuf), #0);
  ByteStream := TIpByteStream.Create(InStream);
  try
    while ByteStream.Read(Byte(CurChar)) and not Abort do begin
      if (Ord(CurChar) in [33..60, 62..126]) then begin
        WordBuf[W] := CurChar;
        Inc(W);
        if W > 74 then AddWordToOutBuf;
      end else if (CurChar = ' ') or (CurChar = #9) then begin
        WordBuf[W] := CurChar;
        Inc(W);
        AddWordToOutBuf;
      end else if (CurChar = #13) then begin
        AddWordToOutBuf;
        SendLine;
      end else if (CurChar = #10) then begin
        { Do nothing }
      end else begin
        AddHexToWord(Byte(CurChar));
      end;
      DoOnCodingProgress(ByteStream.Position, ByteStream.Size, Abort);
    end;
  finally
    ByteStream.Free;
  end;
end;

{ Encode InStream to OutStream - UUEncode }
procedure TIpMimeEntity.EncodeUUEncode(InStream : TStream;
                                       const aFileName : string);
var
  I, O, Count, Temp : Byte;
  InBuf  : array[1..45] of Byte;
  OutBuf : array[0..63] of AnsiChar;
  Abort : Boolean;
begin
  Abort := False;
  FBody.WriteLine('begin 600 ' + aFileName);

  { Encode and stream the attachment }
  repeat
    Count := InStream.Read(InBuf, SizeOf(InBuf));
    if Count <= 0 then Break;
    I := 1;
    O := 0;
    OutBuf[O] := AnsiChar(IpUUTable[Count and $3F]);
    Inc(O);
    while I+2 <= Count do begin
      { Encode 1st byte }
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(IpUUTable[Temp and $3F]);

      { Encode 1st/2nd byte }
      Temp := (InBuf[I] shl 4) or (InBuf[I+1] shr 4);
      OutBuf[O+1] := AnsiChar(IpUUTable[Temp and $3F]);

      { Encode 2nd/3rd byte }
      Temp := (InBuf[I+1] shl 2) or (InBuf[I+2] shr 6);
      OutBuf[O+2] := AnsiChar(IpUUTable[Temp and $3F]);

      { Encode 3rd byte }
      Temp := (InBuf[I+2] and $3F);
      OutBuf[O+3] := AnsiChar(IpUUTable[Temp]);

      Inc(I, 3);
      Inc(O, 4);
    end;

    { Are there odd bytes to add? }
    if (I <= Count) then begin
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(IpUUTable[Temp and $3F]);

      { One odd byte }
      if (I = Count) then begin
        Temp := (InBuf[I] shl 4) and $30;
        OutBuf[O+1] := AnsiChar(IpUUTable[Temp and $3F]);
        Inc(O, 2);
      { Two odd bytes }
      end else begin
        Temp := ((InBuf[I] shl 4) and $30) or ((InBuf[I+1] shr 4) and $0F);
        OutBuf[O+1] := AnsiChar(IpUUTable[Temp and $3F]);
        Temp := (InBuf[I+1] shl 2) and $3C;
        OutBuf[O+2] := AnsiChar(IpUUTable[Temp and $3F]);
        Inc(O, 3);
      end;
    end;

    { Add CR/LF }
    OutBuf[O] := #13;
    OutBuf[O+1] := #10;

    { Write line to stream }
    FBody.Write(OutBuf, (O + 2));
    DoOnCodingProgress(InStream.Position, InStream.Size, Abort);
  until (Count < SizeOf(InBuf)) or Abort;

  { Add terminating end }
  FBody.WriteLine('`');
  FBody.WriteLine('end');
end;

{ Translate each 3 bytes into 4 hextets and encode according to table }
procedure TIpMimeEntity.OctetStreamToHextetStream(InStream : TStream;
                                                  OutStream : TIpAnsiTextStream;
                                                  const Table;
                                                  PadChar, Delim : AnsiChar);
var
  OutBuf: array[0..MaxLineEncode-1] of Char;                           {!!.12}{!!.13}
  OutBufLen: Integer;                                                  {!!.12}
  Abort : Boolean;

  procedure FlushOutBuf;
    {- write out encoded buffer to message stream }
  begin
    if OutBufLen > 0 then begin                                        {!!.12}
      OutStream.WriteLineArray(OutBuf, OutBufLen);
      OutBufLen := 0;                                                  {!!.12}
    end;
  end;

  procedure OutChar(ch : AnsiChar);
    {- buffer the character to go out }
  begin
    if OutBufLen >= MaxLineEncode - 1 then                             {!!.12}{!!.13}
      FlushOutBuf;
    OutBuf[OutBufLen] := Ch;                                           {!!.12}
    inc(OutBufLen);                                                    {!!.12}
  end;

type
  TBuffer = array[0..MaxInt-1] of Byte;
var
  Buffer: ^TBuffer;
  I, Count: Cardinal;
begin
  if InStream is TMemoryStream then
    Buffer := (InStream as TMemoryStream).Memory
  else
    if InStream is TIpMemMapStream then
      Buffer := (InStream as TIpMemMapStream).Memory
    else
      raise EIpBaseException.Create(SNoMemoryStreamErr);

  Abort := False;
  OutBufLen := 0;                                                      {!!.12}
  if (Delim <> #0) then
    OutChar(Delim);

  { Encode and stream the attachment }
  I := 0;
  Count := InStream.Size div 3 * 3;
  while I < Count do
  begin
    { Encode 1st byte }
    OutBuf[OutBufLen] := Char(TIp6BitTable(Table)[Buffer[I] shr 2]);

    { Encode 1st/2nd byte }
    OutBuf[OutBufLen+1] := Char(TIp6BitTable(Table)[((Buffer[I] shl 4) or (Buffer[I+1] shr 4)) and $3F]);

    { Encode 2nd/3rd byte }
    OutBuf[OutBufLen+2] := Char(TIp6BitTable(Table)[((Buffer[I+1] shl 2) or (Buffer[I+2] shr 6)) and $3F]);

    { Encode 3rd byte }
    OutBuf[OutBufLen+3] := Char(TIp6BitTable(Table)[Buffer[I+2] and $3F]);

    Inc(OutBufLen, 4);
    if OutBufLen >= MaxLineEncode - 1 then                             {!!.12}{!!.13}
    begin
      FlushOutBuf;
      if i mod 100 = 0 then
        DoOnCodingProgress(I, Count, Abort);
      if Abort then
        break;
    end;
    Inc(I, 3);
  end;

  Count := InStream.Size;
  { Are there odd bytes to add? }
  if (I < Count) then begin
    OutChar(TIp6BitTable(Table)[Buffer[I] shr 2]);

    { One odd byte }
    if I = Count-1 then begin
      OutChar(TIp6BitTable(Table)[(Buffer[I] shl 4) and $30]);

      if (PadChar <> #0) then
        OutChar(PadChar);
    { Two odd bytes }
    end else begin
      OutChar(TIp6BitTable(Table)[((Buffer[I] shl 4) and $30) or (((Buffer[I+1] shr 4) and $0F)) and $3F]);
      OutChar(TIp6BitTable(Table)[(Buffer[I+1] shl 2) and $3C]);
    end;
    { Add padding }
      if (PadChar <> #0) then
        OutChar(PadChar);
  end;

  if (Delim <> #0) then
    OutChar(Delim);
  FlushOutBuf;
end;

{Begin !!.12}
procedure TIpMIMEEntity.ReadBody(InStream : TIpAnsiTextStream; const StartLine : string);
var
  S : string;
begin
  S := StartLine;
  { read in message body up to message terminator '.' }
  {while not ((S = '.') or AtEndOfStream) do begin}
  while not InStream.AtEndOfStream do begin
    Body.WriteLine(S);
    S := InStream.ReadLine;
  end;
  { write final line }
  Body.WriteLine(S);
end;
{End !!.12}


{ TIpMessage }
constructor TIpMessage.CreateMessage;
begin
  inherited Create(nil);
  FBCC        := TStringList.Create;
  FCC         := TStringList.Create;
  FNewsgroups := TStringList.Create;
  FPath       := TStringList.Create;
  FReceived   := TStringList.Create;
  FRecipients := TStringList.Create;
  FReferences := TStringList.Create;
  FUserFields := TStringList.Create;
  FHeaders    := TIpHeaderCollection.Create (Self);                    {!!.12}
  MsgStream := TIpAnsiTextStream.CreateEmpty;
  NewMessageStream;
end;

destructor TIpMessage.Destroy;
begin
  Clear;
  FBCC.Free;
  FCC.Free;
  FNewsgroups.Free;
  FPath.Free;
  FReceived.Free;
  FRecipients.Free;
  FReferences.Free;
  FUserFields.Free;
  FHeaders.Free;                                                         {!!.12}
  MsgStream.FreeStream;
  MsgStream.Free;
  inherited Destroy;
end;

{Begin !!.13}
procedure TIpMessage.CheckAllHeaders;
var
  i         : Integer;
  j         : Integer;
  HeaderNum : Integer;
begin
  FAttachmentCount := 0;

  { Roll through the list of headers specifically handled by iPRO.
    When one is found, move it into the data structure specific to that
    header field. }
  for i := 0 to IpMaxHeaders - 1 do begin
    if (IpHeaderXRef[i].FieldType = htUserFields) or
       (IpHeaderXRef[i].FieldType = htReceived) then begin
      for j := 0 to Headers.Count - 1 do begin
        if StrLIComp (PChar (IpHeaderXRef[i].FieldString),
                      PChar (Headers.Items[j].Name),
                      Length (IpHeaderXRef[i].FieldString)) = 0 then
          CheckHeaderType (Headers.Items[j],
                           IpHeaderXRef[i].FieldType);
      end;
      
    end else begin
      HeaderNum := Headers.HasHeader (IpHeaderXRef[i].FieldString);
      if HeaderNum >= 0 then
        CheckHeaderType (Headers.Items[HeaderNum],
                         IpHeaderXRef[i].FieldType);
    end;
  end;
end;

procedure TIpMessage.CheckHeaderType (HeaderInfo : TIpHeaderItem;
                                      HeaderType : TIpHeaderTypes);

  function ExtractSingleHeader(HeaderInfo : TIpHeaderItem) : string;
  begin
    Result := Trim(HeaderInfo.Value.Text);
    HeaderInfo.IsProperty := True;                                     {!!.13}
  end;

  procedure ExtractCSVHeader(HeaderInfo : TIpHeaderItem;
                         var AList      : TStringList);
  var
    WorkString : string;
  begin
    WorkString := ExtractSingleHeader(HeaderInfo);
    Parse (WorkString, ',', AList);
    HeaderInfo.IsProperty := True;                                     {!!.13}
  end;

  procedure ExtractListHeader(HeaderInfo : TIpHeaderItem;
                           var AList      : TStringList);
  begin
    AList.Assign (HeaderInfo.Value);
    HeaderInfo.IsProperty := True;                                     {!!.13}
  end;

  procedure ExtractAppendListHeader(HeaderInfo : TIpHeaderItem;
                              const IncludeName : Boolean;             {!!.13}
                                var AList      : TStringList);
  var
    i : Integer;
  begin
    for i := 0 to HeaderInfo.Value.Count - 1 do
{Begin !!.13}
      if IncludeName then
        AList.Add (HeaderInfo.Name + ': ' + HeaderInfo.Value[i])
      else
        AList.Add (HeaderInfo.Value[i]);
    HeaderInfo.IsProperty := True;
{End !!.13}
  end;

begin
  case HeaderType of
    htBCC             :
      ExtractCSVHeader(HeaderInfo, FBCC);
    htCC              :
      ExtractCSVHeader(HeaderInfo, FCC);
    htControl         :
      FControl := ExtractSingleHeader(HeaderInfo);
    htDate            :
      FDate := ExtractSingleHeader(HeaderInfo);
    htDispositionNotify :
      FDispositionNotify := ExtractSingleHeader(HeaderInfo);
    htFrom            :
      FFrom := ExtractSingleHeader(HeaderInfo);
    htFollowUp        :
      FFollowUpTo := ExtractSingleHeader(HeaderInfo);
    htInReplyTo       :
      FInReplyTo := ExtractSingleHeader(HeaderInfo);
    htKeywords        :
      FKeywords := ExtractSingleHeader(HeaderInfo);
    htMessageID       :
      FMessageID := ExtractSingleHeader(HeaderInfo);
    htNewsgroups      :
      ExtractCSVHeader(HeaderInfo, FNewsgroups);
    htNNTPPostingHost :
      FNNTPPostingHost := ExtractSingleHeader(HeaderInfo);
    htOrganization    :
      FOrganization := ExtractSingleHeader(HeaderInfo);
    htPath            :
      ExtractListHeader(HeaderInfo, FPath);
    htPostingHost     :
      FPostingHost := ExtractSingleHeader(HeaderInfo);
    htReceived        :
      ExtractAppendListHeader(HeaderInfo, False, FReceived);           {!!.13}
    htReferences      :
      ExtractListHeader(HeaderInfo, FReferences);
    htReplyTo         :
      FReplyTo := ExtractSingleHeader(HeaderInfo);
    htReturnPath      :
      FReturnPath := ExtractSingleHeader(HeaderInfo);
    htSender          :
      FSender := ExtractSingleHeader(HeaderInfo);
    htSubject         :
      FSubject := ExtractSingleHeader(HeaderInfo);
    htTo              :
      ExtractCSVHeader(HeaderInfo, FRecipients);
    htUserFields      :
      ExtractAppendListHeader(HeaderInfo, True, FUserFields);          {!!.13}
    htXIpro           : begin
    end;
  end;
end;
{End !!.12}

{ Clear properties and free message stream }
procedure TIpMessage.Clear;
begin
  inherited Clear;

  FAttachmentCount := 0;                                               {!!.12}
  FMessageTag := 0;                                                    {!!.15}

  FBCC.Clear;
  FCC.Clear;
  FDate := '';
  FDispositionNotify := '';                                            {!!.12}
  FFrom := '';
  FInReplyTo := '';
  FKeywords := '';
  FFollowupTo := '';                                                   {!!.15}
  FControl := '';                                                      {!!.15}
  FMessageID := '';
  FNewsgroups.Clear;
  FNNTPPostingHost := '';
  FOrganization := '';
  FPath.Clear;
  FPostingHost := '';
  FReceived.Clear;
  FRecipients.Clear;
  FReferences.Clear;
  FReplyTo := '';
  FReturnPath := '';
  FSender := '';
  FSubject := '';
  FUserFields.Clear;
  FHeaders.Clear;                                                      {!!.15}
  MsgStream.FreeStream;
end;

{Begin !!.12}
{ Get headers, body, and MIME parts (if any) }
procedure TIpMessage.DecodeMessage;

var
  AttDepth     : Integer;

  function IsAttachmentStart (const s : string) : Boolean;
  type
    TAttState = (asBegin, asHaveBegin,
                 asNumber1, asNumberSp,
                 asOpenCurly, asNumber2, asNumber2Sp, asCloseCurly,
                 asQuote1, asDblQuote1, AsAlnum1);

  var
    State : TAttState;
    i     : Integer;
    SLen  : Integer;

  begin
    Result := False;
    State  := asBegin;
    i      := 1;
    SLen   := Length (s);

    while i < SLen do begin
      case State of
        asBegin     : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if LowerCase (Copy (s, i, 5)) = 'begin' then begin
            State := asHaveBegin;
            Inc (i, 5);
          end else
            Break;
        end;

        asHaveBegin : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if s[i] = '{' then begin
            Inc (i);
            State := asNumber2;
          end else if s[i] in ['0'..'9'] then begin
            Inc (i);
            State := asNumber1;
          end else
            Break;
        end;

        asNumber1 : begin
          if s[i] in ['0'..'9'] then
            Inc (i)
          else if s[i] in [' ', #09] then begin
            Inc (i);
            State := asNumberSp;
          end else
            Break;
        end;

        asNumberSp : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if s[i] = '"' then begin
            Inc (i);
            State := asDblQuote1;
          end else if s[i] = '''' then begin
            Inc (i);
            State := asQuote1;
          end else if s[i] in ['!'..'~'] then begin
            Inc (i);
            State := asAlNum1;
          end else
            Break;
        end;

        asOpenCurly : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if s[i] in ['0'..'9'] then begin
            Inc (i);
            State := asNumber2;
          end else
            Break;
        end;

        asNumber2 : begin
          if s[i] in ['0'..'9'] then
            Inc (i)
          else if s[i] in [' ', #09] then begin
            Inc (i);
            State := asNumber2Sp;
          end else if s[i] = '}' then begin
            State := asCloseCurly;
            Inc (i);
          end else
            Break;
        end;

        asNumber2Sp : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if s[i] = '}' then begin
            Inc (i);
            State := asCloseCurly;
          end else
            Break;
        end;

        asCloseCurly : begin
          if s[i] in [' ', #09] then
            Inc (i)
          else if s[i] = '"' then begin
            Inc (i);
            State := asDblQuote1;
          end else if s[i] = '''' then begin
            Inc (i);
            State := asQuote1;
          end else
            Break;
        end;

        asQuote1 : begin
          if s[i] in [' '..'&', '('..'~'] then
            Inc (i)
          else if s[i] = '''' then begin
            Result := True;
            Break;
          end else
            Break;
        end;

        asDblQuote1 : begin
          if s[i] in [' '..'!', '#'..'~'] then
            Inc (i)
          else if s[i] = '"' then begin
            Result := True;
            Break;
          end else
            Break;
        end;

        AsAlnum1 : begin
          if s[i] in ['!'..'~'] then begin
            Result := True;
            Break;
          end else
            Break;
        end;

      end;
    end;
  end;

  function IsAttachmentEnd (const s : string) : Boolean;
  begin
    if LowerCase (Copy (s, 1, 3)) = 'end' then
      Result := True
    else
      Result := False;
  end;

  procedure CheckForAttachment (const s : string);
  begin
    if IsAttachmentStart (s) then begin
      if AttDepth = 0 then
        Inc (FAttachmentCount);
      Inc (AttDepth);
    end else if (IsAttachmentEnd (s)) and
                (FAttachmentCount > 0) then
      Dec (AttDepth);
  end;
{End !!.12}
var
  RawHeaders : TStringList;
  S : string;
  i, j : Integer;                                                      {!!.13}
begin
  { get message headers}
  Position := 0;
  RawHeaders := TStringList.Create;
  try
    S := ReadLine;
    repeat
      if S <> '' then                                                  {!!.15}
        RawHeaders.Add(S);
      S := ReadLine;
    until (S = '');

    FHeaders.Clear;                                                    {!!.12}
    FHeaders.LoadHeaders (RawHeaders, False);                          {!!.12}
    CheckAllHeaders;                                                   {!!.12}

    { decode MIME headers }
    DecodeMimeHeaders(RawHeaders);

{Begin !!.13}
    { If this is a MIME message, mark the MIME headers as being exposed via an
      iPRO property. }
    if FIsMime then
      for i := Low(IpMimeHeaders) to High(IpMimeHeaders) do begin
        j := FHeaders.HasHeader(IpMimeHeaders[i]);
        if j > -1 then
          FHeaders.Items[j].IsProperty := True;
      end;
{End !!.13}
  finally
    RawHeaders.Free;
  end;

  { if message is mime, then decode mime parts }
  if IsMime then begin                                                 {!!.01}
    if (FContentDispositionType = strAttachment) then begin            {!!.12}
      Inc (FParent.FAttachmentCount);                                  {!!.12}{!!.15}
      DecodeEntityAsAttachment(MsgStream)                              {!!.01}
    end else                                                           {!!.12}
      DecodeEntity(MsgStream);
  end else begin
    { otherwise, just read in the message body. }
    repeat  { skip over blank lines between headers and body }
      S := ReadLine;
    until (S <> '') or AtEndOfStream;

    { read in message body up to message terminator '.' }
    {while not ((S = '.') or AtEndOfStream) do begin}                  {!!.10}
    while not AtEndOfStream do begin                                   {!!.10}
      Body.WriteLine(S);
      AttDepth := 0;                                                   {!!.12}
      CheckForAttachment (S);                                          {!!.12}
      S := ReadLine;
    end;
    { write final line }                                               {!!.10}
    if S <> '' then                                                    {!!.13}
      Body.WriteLine(S);                                               {!!.10}

{Begin !!.12}
    { Read the message body. }
    {ReadBody(MsgStream, S); }
{End !!.12}
  end;
  Body.Position := 0;
end;

{ Build message stream with headers, body, and MIME parts (if any) }
procedure TIpMessage.EncodeMessage;
var
  i : Integer;
  Size : Longint;                                                      {!!.12}
  FileName : string;                                                   {!!.12}
  Strm : TIpMemMapStream;                                              {!!.12}
  RawHeaders : TStringList;
begin
  NewMessageStream;
{Begin !!.12}
  { If we have some very large attachments then we need to use a memory mapped
    file stream instead of TMemory, in order to improve performance. }
  Size := 0;
  for i := 0 to Pred(FMimeParts.Count) do
    inc(Size, FMimeParts[i].FOriginalSize);
  if Size > IpLgAttachSizeBoundry then begin
    MsgStream.FreeStream;
    FileName := GetTemporaryFile(GetTemporaryPath);
    if FileExistsUTF8(FileName) then
      DeleteFileUTF8(FileName);
    Strm := TIpMemMapStream.Create(FileName, False, True);
    Strm.Size := Trunc(Size * 1.5);
    Strm.Open;
    MsgStream.Stream := Strm;
  end;
{End !!.12}
  if (FContentType <> '') then begin
    FIsMime := True;
    FMimeVersion := '1.0';
  end;
  RawHeaders := TStringList.Create;
  try
    EncodeSingleHeader(strReturnPath, RawHeaders, FReturnPath);
    EncodeMultiHeader(strReceived, RawHeaders, FReceived, #09, True);
    EncodeListHeader(strPath, RawHeaders, FPath, ',', True);
    EncodeListHeader(strNewsgroups, RawHeaders, FNewsgroups, ',', False); {!!.14}
    EncodeSingleHeader(strMessageID, RawHeaders, FMessageID);
    EncodeSingleHeader (strDispositionNotify, RawHeaders,                {!!.12}
                        FDispositionNotify);                             {!!.12}
    EncodeSingleHeader(strReplyTo, RawHeaders, FReplyTo);
    EncodeSingleHeader(strFrom, RawHeaders, FFrom);
    EncodeListHeader(strTo, RawHeaders, FRecipients, ',', True);
    EncodeSingleHeader(strSubject, RawHeaders, FSubject);
    EncodeSingleHeader(strDate, RawHeaders, FDate);
    EncodeSingleHeader(strOrganization, RawHeaders, FOrganization);
    EncodeListHeader(strCC, RawHeaders, FCC, ',', False);
    EncodeListHeader(strBCC, RawHeaders, FBCC, ',', False);
    EncodeSingleHeader(strInReplyTo, RawHeaders, FInReplyTo);
    EncodeListHeader(strReferences, RawHeaders, FReferences, '', False);
    EncodeSingleHeader(strSender, RawHeaders, FSender);
    EncodeSingleHeader(strKeywords, RawHeaders, FKeywords);
    EncodeMultiHeader('', RawHeaders, FUserFields, Char(0), False);
    EncodeSingleHeader(strControl, RawHeaders, FControl);               {!!.12}
    EncodeSingleHeader(strFollowUp, RawHeaders, FFollowupTo);           {!!.12}
{Begin !!.13}
    for i := 0 to Pred(Headers.Count) do
      { Write the header out only if it is not a header exposed via an iPRO
        property. }
      if (not Headers.Items[i].IsProperty) then begin
        if Headers.Items[i].Value.Count = 1 then
          EncodeSingleHeader(Headers.Items[i].Name + ': ', RawHeaders,
                             Headers.Items[i].Value[0])
        else
          EncodeMultiheader(Headers.Items[i].Name + ': ', RawHeaders,
                            Headers.Items[i].Value, #09, True);
      end;
{End !!.13}
    if IsMime then
      EncodeMimeHeaders(RawHeaders);
    if (RawHeaders.Count = 0) then
      Exit;
    for i := 0 to Pred(RawHeaders.Count) do
      WriteLine(RawHeaders[i]);
  finally
    RawHeaders.Free;
  end;

{Begin !!.13}
  WriteLine('');
  if IsMime then
    EncodeEntity(MsgStream)
  else if (FBody.Size > 0) then begin
    FBody.Position := 0;
    repeat
      WriteLine(Body.ReadLine);
    until FBody.AtEndOfStream;
  end;  { if }
{End !!.13}
end;

{ Load message from file stream and decode }
procedure TIpMessage.LoadFromFile(const aFileName : string);
{Begin !!.12}
var
  SourceStream : TIpMemMapStream;
{End !!.12}
begin
  Clear;
  NewMessageStream;                                                    {!!.03}
{Begin !!.12}
  SourceStream := TIpMemMapStream.Create(aFileName, True, False);
  try
    SourceStream.Open;
{Begin !!.15}
    if SourceStream.Size > IpLgAttachSizeBoundry then begin
      MsgStream.FreeStream;
      MsgStream.Stream := SourceStream;
    end
    else
      MsgStream.CopyFrom(SourceStream, 0);
  finally
    if MsgStream.Stream <> SourceStream then
      SourceStream.Free;
{End !!.15}
  end;
{End !!.12}

  try                                                                  {!!.03}
    DecodeMessage;
  except                                                               {!!.03}
    { just eat the exception, the messge might be corrupt, but the }
    { raw text (MessageStream property) will still be available    }
  end;                                                                 {!!.03}
end;

{Begin !!.12}
procedure TIpMessage.LoadFromStream(aStream : TStream);
var
  FileName : string;
  Strm : TIpMemMapStream;
begin
  Clear;
  NewMessageStream;
  if aStream.Size > IpLgAttachSizeBoundry then begin
    MsgStream.FreeStream;
    FileName := GetTemporaryFile(GetTemporaryPath);
    if FileExistsUTF8(FileName) then
      DeleteFileUTF8(FileName);
    Strm := TIpMemMapStream.Create(FileName, False, True);
    Strm.Size := aStream.Size;
    Strm.Open;
    MsgStream.Stream := Strm;
  end;
  MsgStream.CopyFrom(aStream, 0);

  try
    DecodeMessage;
  except
    { just eat the exception, the messge might be corrupt, but the }
    { raw text (MessageStream property) will still be available    }
  end;                                                               
end;


{ Create new message stream but retain existing decoded message }
procedure TIpMessage.NewMessageStream;
begin
  MsgStream.FreeStream;
  MsgStream.Stream := TMemoryStream.Create;
  MsgStream.bsInitForNewStream;                                        {!!.02}
end;

{ Clear all and create new empty message stream }
procedure TIpMessage.NewMessage;
begin
  Clear;
  NewMessageStream;
end;

{ Position property read access method }
function TIpMessage.GetPosition : Longint;
begin
  if Assigned(MsgStream) then
    Result := MsgStream.Position
  else
    Result := 0;
end;

{ Size property read access method }
function TIpMessage.GetSize : Longint;
begin
  if Assigned(MsgStream) then
    Result := MsgStream.Size
  else
    Result := 0;
end;

{ Return next line from the message stream (CRLF stripped) }
function TIpMessage.ReadLine : string;
begin
  if Assigned(MsgStream) then
    Result := MsgStream.ReadLine
  else
    Result := '';
end;

{ Return next line from the message stream (CRLF retained) }
function TIpMessage.ReadLineCRLF : string;
begin
  if Assigned(MsgStream) then
    Result := MsgStream.ReadLine + CRLF
  else
    Result := '';
end;

{- Save raw message stream to file }
procedure TIpMessage.SaveToFile(const aFileName : string);
var
  FS : TFileStream;
begin
  EncodeMessage;
  Position := 0;
  FS := TFileStream.Create(UTF8ToSys(aFileName), fmCreate);
  try
    FS.CopyFrom(MsgStream, MsgStream.Size);
  finally
    FS.Free;
  end;
end;

{Begin !!.12}
{- Save raw message stream }
procedure TIpMessage.SaveToStream(Stream: TStream);
begin
  Position := 0;
  Stream.CopyFrom(MsgStream, MsgStream.Size);
end;

procedure TIpMessage.SetHeaders(Headers : TIpHeaderCollection);
begin
  FHeaders.Assign(Headers);
end;
{End !!.12}

{ Position property write access method }
procedure TIpMessage.SetPosition(Value : Longint);
begin
  if Assigned(MsgStream) then
    MsgStream.Position := Value;
end;

{ Write string onto the message stream and append CRLF terminator }
procedure TIpMessage.WriteLine(const aSt : string);
begin
  if Assigned(MsgStream) then
    MsgStream.WriteLine(aSt);
end;

{ Indicates whether or not we're at the end of the message stream }
function TIpMessage.AtEndOfStream : Boolean;
begin
  if Assigned(MsgStream) then
    Result := MsgStream.AtEndOfStream
  else
    Result := True;
end;

{ Return 'alternative' text/plain mime part }
function TIpMessage.GetBodyPlain(CanCreate : Boolean) : TIpMimeEntity;
var
  aParent : TIpMimeEntity;
begin
  aParent := FindNestedMimePart(strMultipart, strAlternative, '');   {!!.02}
  if not Assigned(aParent) then
    aParent := Self;
{Begin !!.15}
  Result := aParent.FindNestedMimePart(strText, strPlain, '');
  if (Result = nil) and CanCreate then begin
    Result := NewMimePart;
    Result.ContentType := strText;
    Result.ContentSubtype := strPlain;
  end;
{End !!.15}
end;

{ Return 'alternative' text/html mime part }
function TIpMessage.GetBodyHtml(CanCreate : Boolean) : TIpMimeEntity;
var
  aParent : TIpMimeEntity;
begin
  aParent := FindNestedMimePart(strMultipart, strAlternative, '');   {!!.02}
  if not Assigned(aParent) then
    aParent := Self;
{Begin !!.15}
  Result := aParent.FindNestedMimePart(strText, strHtml, '');
  if (Result = nil) and CanCreate then begin
    Result := NewMimePart;
    Result.ContentType := strText;
    Result.ContentSubtype := strHTML;
  end;
{End !!.15}
end;

{ Add a file attachment using default types }
procedure TIpMessage.AddDefaultAttachment(const aFileName: string);     {!!.02}
begin
  with NewMimePart do begin
    EntityName := ExtractFileName(aFileName);
    ContentDispositionType := 'attachment';
    EncodeBodyFile(aFileName);
  end;
end;

procedure TIpMessage.AddDefaultAttachmentAs (const aFileName      : string;  {!!.12}
                                             const AttachmentName : string); {!!.12}
begin                                                                    {!!.12}
  with NewMimePart do begin                                              {!!.12}
    EntityName := ExtractFileName (AttachmentName);                      {!!.12}
    ContentDispositionType := 'attachment';                              {!!.12}
    EncodeBodyFile (aFileName);                                          {!!.12}
  end;                                                                   {!!.12}
end;                                                                     {!!.12}

{ Set message properties from another TIpMessage }
procedure TIpMessage.Assign(Source: TPersistent);
var
  SourcePos : Integer;
  SourceMsg : TIpMessage;
begin
  if Source is TIpMessage then begin
    SourceMsg := TIpMessage(Source);
    { clear our streams and properties }
    NewMessage;
    { ensure we are at the beginning of our streams }
    Position := 0;
    SourcePos := SourceMsg.Position;
    SourceMsg.Position := 0;
    MsgStream.CopyFrom(SourceMsg.MsgStream, 0);
    Position := 0;
    SourceMsg.Position := SourcePos;
    try                                                                {!!.03}
      DecodeMessage;
    except                                                             {!!.03}
      { just eat the exception, the messge might be corrupt, but the }
      { raw text (MessageStream property) will still be available    }
    end;                                                               {!!.03}
  end else
    inherited Assign(Source);
end;

procedure TIpMessage.SetBCC(const Value: TStringList);                 {!!.01}
begin
  FBCC.Assign(Value);
end;

procedure TIpMessage.SetCC(const Value: TStringList);                  {!!.01}
begin
  FCC.Assign(Value);
end;

procedure TIpMessage.SetNewsgroups(const Value: TStringList);          {!!.01}
begin
  FNewsgroups.Assign(Value);
end;

procedure TIpMessage.SetPath(const Value: TStringList);                {!!.01}
begin
  FPath.Assign(Value);
end;

procedure TIpMessage.SetReceived(const Value: TStringList);            {!!.01}
begin
  FReceived.Assign(Value);
end;

procedure TIpMessage.SetRecipients(const Value: TStringList);          {!!.01}
begin
  FRecipients.Assign(Value);
end;

procedure TIpMessage.SetReferences(const Value: TStringlist);          {!!.01}
begin
  FReferences.Assign(Value);
end;

procedure TIpMessage.SetUserFields(const Value: TStringList);          {!!.01}
begin
  FUserFields.Assign(Value);
end;



{ TIpFormDataEntity }
constructor TIpFormDataEntity.Create(ParentEntity : TIpMimeEntity);
begin
  inherited Create(ParentEntity);
  ContentType := strMultipart;
  ContentSubType := strFormData;
  Boundary := GenerateBoundary;
end;

destructor TIpFormDataEntity.Destroy;
begin
  inherited Destroy;
end;

{ Add file as nested Mime part of FilesEntity block }
procedure TIpFormDataEntity.AddFile(const aFileName,
                                    aContentType,
                                    aSubtype : string;
                                    aEncoding : TIpMimeEncodingMethod);
var
  Blk : TIpMimeEntity;
  MS : TIpMemMapStream;
begin
  if not Assigned(FFilesEntity) then begin
    FFilesEntity := NewMimePart;
    FFilesEntity.EntityName := strFiles;
    FFilesEntity.ContentDispositionType := strFormData;
    FFilesEntity.ContentType := strMultipart;
    FFilesEntity.ContentSubtype := strMixed;
  end;

  Blk := FFilesEntity.NewMimePart;
  Blk.ContentDispositionType := strAttachment;
  Blk.ContentType := aContentType;
  Blk.ContentSubtype := aSubtype;
  Blk.ContentTransferEncoding := aEncoding;

  MS := TIpMemMapStream.Create(aFileName, True, False);
  try
    MS.Open;
    Blk.EncodeBodyStream(MS, aFileName);
  finally
    MS.Free;
  end;
end;

{ Add FormData Mime part }
procedure TIpFormDataEntity.AddFormData(const aName, aText : string);
var
  Blk : TIpMimeEntity;
begin
  Blk := NewMimePart;
  Blk.EntityName := aName;
  Blk.ContentDispositionType := strFormData;
  Blk.Body.WriteLine(aText);
end;

{ Generate raw Mime message and save to stream }
procedure TIpFormDataEntity.SaveToStream(aStream : TStream);
var
  TS : TIpAnsiTextStream;
  SL : TStringList;
begin
  TS := TIpAnsiTextStream.Create(aStream);
  try
    SL := TStringList.Create;
    try
      EncodeMimeHeaders(SL);
      SL.SaveToStream(TS);
      EncodeEntity(TS);
    finally
      SL.Free;
    end;
  finally
    TS.Free;
  end;
end;

{HTTP Authentication Support -- .02}
function IpBase64EncodeString(const InStr: string): string;              {!!.03}
{
encode a string into Base64, intended for producing short ( < 100 chars or so)
coded strings to be passed as part of HTTP authentications via HTTP headers.

NO LINE ORIENTED SMARTS: if you need to work with blocks of text use the
IpMsg class
}
var
  CvtBuff: PChar;
  I, Ct, Count, OutLen: Cardinal;

function CodeByte(byt : Byte) : char;
{- encode 6-bit value to BinHex char and send it }
begin
  Result := Ip64Table[byt and $3F];
end;

begin
  Result := '';
  Count := Length(InStr);
  if Count = 0 then // empty input string nothing to encode              {!!.03}
    Exit;                                                                {!!.03}
  OutLen := Count * 2; // leave plenty of room for encoded string        {!!.03}
  GetMem(CvtBuff, OutLen + 1);

  Ct := 0;
  I := 1;

  if Count >= 3 then begin                                               {!!.03}
    while I <= (Count - 2) do begin
      { Encode 1st byte }
      CvtBuff[Ct] := CodeByte(Ord(InStr[I]) shr 2);
      Inc(Ct);

      { Encode 1st/2nd byte }
      CvtBuff[Ct]  := CodeByte((Ord(InStr[I]) shl 4) or (Ord(InStr[I+1]) shr 4));
      Inc(Ct);

      { Encode 2nd/3rd byte }
      CvtBuff[Ct] := CodeByte((Ord(InStr[I+1]) shl 2) or (Ord(InStr[I+2]) shr 6));
      Inc(Ct);

      { Encode 3rd byte }
      CvtBuff[Ct] := CodeByte(Ord(InStr[I+2]) and $3F);
      Inc(Ct);

      Inc(I, 3);
    end;
  end;                                                                   {!!.03}

  { Are there odd bytes to add? }
  if (I <= Count) then begin
    CvtBuff[Ct] := CodeByte(Ord(InStr[I]) shr 2);
    Inc(Ct);

    { One odd byte }
    if I = Count then begin
      CvtBuff[Ct] := CodeByte((Ord(InStr[I]) shl 4) and $30);
      Inc(Ct);
      CvtBuff[Ct] := '='; // pad char
      Inc(Ct);
    { Two odd bytes }
    end else begin
      CvtBuff[Ct] := CodeByte(((Ord(InStr[I]) shl 4) and $30)
        or ((Ord(InStr[I+1]) shr 4) and $0F));
      Inc(Ct);
      CvtBuff[Ct] := CodeByte((Ord(InStr[I+1]) shl 2) and $3C);
      Inc(Ct);
    end;
    { Add padding }
      CvtBuff[Ct] := '=';
      Inc(Ct);
  end;

  CvtBuff[Ct] := #0;
  Result := StrPas(CvtBuff);
  FreeMem(CvtBuff, OutLen + 1);
end;


end.
