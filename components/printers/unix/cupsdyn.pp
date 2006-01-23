{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    This code provide an interface with Common UNIX Printing System (CUPS).
    it is based on cups.h version 1.47
    This code is tested with the 1.1.19 of CUPS
    
  link:
    http://www.cups.org
    http://localhost:631/spm.html (for documentation)
    
  See : /usr/share/doc/cups/licence.txt
  
  Required
    CUPS 1.1.19 or more
    libCUPS
    
  History
   sept 14 2003 - Create
   nov  04 2003 - First release
   mars 08 2005 - Dynamique link lib by Jesus Reyes (big thanks)
                - Some modifications for work with Mdk 10.1
------------------------------------------------------------------------------}
unit cupsdyn;

{$mode objfpc}{$H+}
{.$define UseLibC}
interface


uses
  Classes, SysUtils, LCLProc, dynlibs,
  {$ifdef UseLibC}
  {$IFDEF darwin}
  miniCupsLibc
  {$ELSE}
  Libc
  {$ENDIF}
  {$else}
  baseunix, unix, sockets
  {$endif}
  ;

{$PACKRECORDS C}

{.$LINKLIB ssl}
{.$LINKLIB crypto}
{.$LINKLIB nsl}

{$ifndef UseLibC}
type
  sockaddr_in = TSockAddr;
  pfd_set = PFDSet;
  PFILE = pointer;
{$endif}

//
//
// CUPS_language types & consts
//
//
type
  //Message Indices
  cups_msg_t=(
    CUPS_MSG_OK,
    CUPS_MSG_CANCEL,
    CUPS_MSG_HELP,
    CUPS_MSG_QUIT,
    CUPS_MSG_CLOSE,
    CUPS_MSG_YES,
    CUPS_MSG_NO,
    CUPS_MSG_ON,
    CUPS_MSG_OFF,
    CUPS_MSG_SAVE,
    CUPS_MSG_DISCARD,
    CUPS_MSG_DEFAULT,
    CUPS_MSG_OPTIONS,
    CUPS_MSG_MORE_INFO,
    CUPS_MSG_BLACK,
    CUPS_MSG_COLOR,
    CUPS_MSG_CYAN,
    CUPS_MSG_MAGENTA,
    CUPS_MSG_YELLOW,
    CUPS_MSG_COPYRIGHT,
    CUPS_MSG_GENERAL,
    CUPS_MSG_PRINTER,
    CUPS_MSG_IMAGE,
    CUPS_MSG_HPGL2,
    CUPS_MSG_EXTRA,
    CUPS_MSG_DOCUMENT,
    CUPS_MSG_OTHER,
    CUPS_MSG_PRINT_PAGES,
    CUPS_MSG_ENTIRE_DOCUMENT,
    CUPS_MSG_PAGE_RANGE,
    CUPS_MSG_REVERSE_ORDER,
    CUPS_MSG_PAGE_FORMAT,
    CUPS_MSG_1_UP,
    CUPS_MSG_2_UP,
    CUPS_MSG_4_UP,
    CUPS_MSG_IMAGE_SCALING,
    CUPS_MSG_USE_NATURAL_IMAGE_SIZE,
    CUPS_MSG_ZOOM_BY_PERCENT,
    CUPS_MSG_ZOOM_BY_PPI,
    CUPS_MSG_MIRROR_IMAGE,
    CUPS_MSG_COLOR_SATURATION,
    CUPS_MSG_COLOR_HUE,
    CUPS_MSG_FIT_TO_PAGE,
    CUPS_MSG_SHADING,
    CUPS_MSG_DEFAULT_PEN_WIDTH,
    CUPS_MSG_GAMMA_CORRECTION,
    CUPS_MSG_BRIGHTNESS,
    CUPS_MSG_ADD,
    CUPS_MSG_DELETE,
    CUPS_MSG_MODIFY,
    CUPS_MSG_PRINTER_URI,
    CUPS_MSG_PRINTER_NAME,
    CUPS_MSG_PRINTER_LOCATION,
    CUPS_MSG_PRINTER_INFO,
    CUPS_MSG_PRINTER_MAKE_AND_MODEL,
    CUPS_MSG_DEVICE_URI,
    CUPS_MSG_FORMATTING_PAGE,
    CUPS_MSG_PRINTING_PAGE,
    CUPS_MSG_INITIALIZING_PRINTER,
    CUPS_MSG_PRINTER_STATE,
    CUPS_MSG_ACCEPTING_JOBS,
    CUPS_MSG_NOT_ACCEPTING_JOBS,
    CUPS_MSG_PRINT_JOBS,
    CUPS_MSG_CLASS,
    CUPS_MSG_LOCAL,
    CUPS_MSG_REMOTE,
    CUPS_MSG_DUPLEXING,
    CUPS_MSG_STAPLING,
    CUPS_MSG_FAST_COPIES,
    CUPS_MSG_COLLATED_COPIES,
    CUPS_MSG_PUNCHING,
    CUPS_MSG_COVERING,
    CUPS_MSG_BINDING,
    CUPS_MSG_SORTING,
    CUPS_MSG_SMALL,
    CUPS_MSG_MEDIUM,
    CUPS_MSG_LARGE,
    CUPS_MSG_VARIABLE,
    CUPS_MSG_IDLE,
    CUPS_MSG_PROCESSING,
    CUPS_MSG_STOPPED,
    CUPS_MSG_ALL,
    CUPS_MSG_ODD,
    CUPS_MSG_EVEN_PAGES,
    CUPS_MSG_DARKER_LIGHTER,
    CUPS_MSG_MEDIA_SIZE,
    CUPS_MSG_MEDIA_TYPE,
    CUPS_MSG_MEDIA_SOURCE,
    CUPS_MSG_ORIENTATION,
    CUPS_MSG_PORTRAIT,
    CUPS_MSG_LANDSCAPE,
    CUPS_MSG_JOB_STATE,
    CUPS_MSG_JOB_NAME,
    CUPS_MSG_USER_NAME,
    CUPS_MSG_PRIORITY,
    CUPS_MSG_COPIES,
    CUPS_MSG_FILE_SIZE,
    CUPS_MSG_PENDING,
    CUPS_MSG_OUTPUT_MODE,
    CUPS_MSG_RESOLUTION,
    CUPS_MSG_TEXT,
    CUPS_MSG_PRETTYPRINT,
    CUPS_MSG_MARGINS,
    CUPS_MSG_LEFT,
    CUPS_MSG_RIGHT,
    CUPS_MSG_BOTTOM,
    CUPS_MSG_TOP,
    CUPS_MSG_FILENAME,
    CUPS_MSG_PRINT,
    CUPS_MSG_OPTIONS_INSTALLED,
    CUPS_MSG_AUTO,
    CUPS_MSG_HTTP_BASE:= 200,
    CUPS_MSG_HTTP_END := 505,
    CUPS_MSG_MAX
  );

  //Language Encodings
  cups_encoding_t=(
    CUPS_US_ASCII,
    CUPS_ISO8859_1,
    CUPS_ISO8859_2,
    CUPS_ISO8859_3,
    CUPS_ISO8859_4,
    CUPS_ISO8859_5,
    CUPS_ISO8859_6,
    CUPS_ISO8859_7,
    CUPS_ISO8859_8,
    CUPS_ISO8859_9,
    CUPS_ISO8859_10,
    CUPS_UTF8,
    CUPS_ISO8859_13,
    CUPS_ISO8859_14,
    CUPS_ISO8859_15,
    CUPS_WINDOWS_874,
    CUPS_WINDOWS_1250,
    CUPS_WINDOWS_1251,
    CUPS_WINDOWS_1252,
    CUPS_WINDOWS_1253,
    CUPS_WINDOWS_1254,
    CUPS_WINDOWS_1255,
    CUPS_WINDOWS_1256,
    CUPS_WINDOWS_1257,
    CUPS_WINDOWS_1258,
    CUPS_KOI8_R,
    CUPS_KOI8_U
   );

   Pcups_lang_str=^cups_lang_str;
   cups_lang_str = record
        next     : Pcups_lang_str;        //Next language in cache
        used     : longint;               //Number of times this entry has been used
        encoding : cups_encoding_t;       //Text encoding
        language : array[0..15] of char;  //Language/locale name
        messages : array[0..Ord(CUPS_MSG_MAX)-1] of Pchar; //Message array
   end;
   cups_lang_t = cups_lang_str;
   Pcups_lang_t=^cups_lang_t;


//
//
// CUPS_HTTP types & consts
//
//
const
  HTTP_MAX_URI	   =	1024;	//Max length of URI string
  HTTP_MAX_HOST	   =	256;	//Max length of hostname string
  HTTP_MAX_BUFFER  =	2048;	//Max length of data buffer
  HTTP_MAX_VALUE   =	256;	//Max header field value length

type
  //----------------------------------------------------------------------------
  //   md5.h
  //----------------------------------------------------------------------------
  Pmd5_byte_t=^md5_byte_t;
  md5_byte_t = byte; //8-bit byte
  md5_word_t=dword; //32-bit word

  //Define the state of the MD5 Algorithm.
  md5_state_s = record
          count : array[0..1] of md5_word_t; //message length in bits, lsw first
          abcd  : array[0..3] of md5_word_t; //digest buffer
          buf   : array[0..63] of md5_byte_t;//accumulate block
       end;
  md5_state_t = md5_state_s;
  {-----------------------------------------------------------------------------}

  //States are server-oriented
  http_state_t=(
    HTTP_WAITING,	//Waiting for command
    HTTP_OPTIONS,	//OPTIONS command, waiting for blank line
    HTTP_GET,		//GET command, waiting for blank line
    HTTP_GET_SEND,	//GET command, sending data
    HTTP_HEAD,		//HEAD command, waiting for blank line
    HTTP_POST,		//POST command, waiting for blank line
    HTTP_POST_RECV,	//POST command, receiving data
    HTTP_POST_SEND,	//POST command, sending data
    HTTP_PUT,		//PUT command, waiting for blank line
    HTTP_PUT_RECV,	//PUT command, receiving data
    HTTP_DELETE,	//DELETE command, waiting for blank line
    HTTP_TRACE,		//TRACE command, waiting for blank line
    HTTP_CLOSE,		//CLOSE command, waiting for blank line
    HTTP_STATUS		//Command complete, sending status
  );

  //HTTP version numbers...
  http_version_t=(
    HTTP_0_9 := 9,    //HTTP/0.9
    HTTP_1_0 := 100,  //HTTP/1.0
    HTTP_1_1 := 101   //HTTP/1.1
  );

  //HTTP keep-alive values...
  http_keepalive_t=(
    HTTP_KEEPALIVE_OFF := 0,
    HTTP_KEEPALIVE_ON
  );

  //HTTP transfer encoding values...
  http_encoding_t=(
    HTTP_ENCODE_LENGTH,   //Data is sent with Content-Length
    HTTP_ENCODE_CHUNKED	  //Data is chunked
  );

  //HTTP encryption values...
  http_encryption_t=(
    HTTP_ENCRYPT_IF_REQUESTED,	//Encrypt if requested (TLS upgrade)
    HTTP_ENCRYPT_NEVER,		//Never encrypt
    HTTP_ENCRYPT_REQUIRED,	//Encryption is required (TLS upgrade)
    HTTP_ENCRYPT_ALWAYS		//Always encrypt (SSL)
  );

  //HTTP authentication types...
  http_auth_t=(
    HTTP_AUTH_NONE,		//No authentication in use
    HTTP_AUTH_BASIC,		//Basic authentication in use
    HTTP_AUTH_MD5,		//Digest authentication in use
    HTTP_AUTH_MD5_SESS,		//MD5-session authentication in use
    HTTP_AUTH_MD5_INT,		//Digest authentication in use for body
    HTTP_AUTH_MD5_SESS_INT	//MD5-session authentication in use for body
  );

  //HTTP status codes...
  http_status_t=(
    HTTP_ERROR := -1,		//An error response from httpXxxx() */

    HTTP_CONTINUE := 100,	//Everything OK, keep going... */
    HTTP_SWITCHING_PROTOCOLS,	//HTTP upgrade to TLS/SSL */

    HTTP_OK := 200,		//OPTIONS/GET/HEAD/POST/TRACE command was successful */
    HTTP_CREATED,		//PUT command was successful */
    HTTP_ACCEPTED,		//DELETE command was successful */
    HTTP_NOT_AUTHORITATIVE,	//Information isn't authoritative */
    HTTP_NO_CONTENT,		//Successful command, no new data */
    HTTP_RESET_CONTENT,		//Content was reset/recreated */
    HTTP_PARTIAL_CONTENT,	//Only a partial file was recieved/sent */

    HTTP_MULTIPLE_CHOICES:=300,	//Multiple files match request */
    HTTP_MOVED_PERMANENTLY,	//Document has moved permanently */
    HTTP_MOVED_TEMPORARILY,	//Document has moved temporarily */
    HTTP_SEE_OTHER,		//See this other link... */
    HTTP_NOT_MODIFIED,		//File not modified */
    HTTP_USE_PROXY,		//Must use a proxy to access this URI */

    HTTP_BAD_REQUEST := 400,	//Bad request */
    HTTP_UNAUTHORIZED,		//Unauthorized to access host */
    HTTP_PAYMENT_REQUIRED,	//Payment required */
    HTTP_FORBIDDEN,		//Forbidden to access this URI */
    HTTP_NOT_FOUND,		//URI was not found */
    HTTP_METHOD_NOT_ALLOWED,	//Method is not allowed */
    HTTP_NOT_ACCEPTABLE,	//Not Acceptable */
    HTTP_PROXY_AUTHENTICATION,	//Proxy Authentication is Required */
    HTTP_REQUEST_TIMEOUT,	//Request timed out */
    HTTP_CONFLICT,		//Request is self-conflicting */
    HTTP_GONE,			//Server has gone away */
    HTTP_LENGTH_REQUIRED,	//A content length or encoding is required */
    HTTP_PRECONDITION,		//Precondition failed */
    HTTP_REQUEST_TOO_LARGE,	//Request entity too large */
    HTTP_URI_TOO_LONG,		//URI too long */
    HTTP_UNSUPPORTED_MEDIATYPE,	//The requested media type is unsupported */
    HTTP_UPGRADE_REQUIRED:= 426,//Upgrade to SSL/TLS required */

    HTTP_SERVER_ERROR := 500,	//Internal server error */
    HTTP_NOT_IMPLEMENTED,	//Feature not implemented */
    HTTP_BAD_GATEWAY,		//Bad gateway */
    HTTP_SERVICE_UNAVAILABLE,	//Service is unavailable */
    HTTP_GATEWAY_TIMEOUT,	//Gateway connection timed out */
    HTTP_NOT_SUPPORTED		//HTTP version not supported */
  );

  //HTTP field names...
  http_field_t=(
    HTTP_FIELD_UNKNOWN := -1,
    HTTP_FIELD_ACCEPT_LANGUAGE,
    HTTP_FIELD_ACCEPT_RANGES,
    HTTP_FIELD_AUTHORIZATION,
    HTTP_FIELD_CONNECTION,
    HTTP_FIELD_CONTENT_ENCODING,
    HTTP_FIELD_CONTENT_LANGUAGE,
    HTTP_FIELD_CONTENT_LENGTH,
    HTTP_FIELD_CONTENT_LOCATION,
    HTTP_FIELD_CONTENT_MD5,
    HTTP_FIELD_CONTENT_RANGE,
    HTTP_FIELD_CONTENT_TYPE,
    HTTP_FIELD_CONTENT_VERSION,
    HTTP_FIELD_DATE,
    HTTP_FIELD_HOST,
    HTTP_FIELD_IF_MODIFIED_SINCE,
    HTTP_FIELD_IF_UNMODIFIED_SINCE,
    HTTP_FIELD_KEEP_ALIVE,
    HTTP_FIELD_LAST_MODIFIED,
    HTTP_FIELD_LINK,
    HTTP_FIELD_LOCATION,
    HTTP_FIELD_RANGE,
    HTTP_FIELD_REFERER,
    HTTP_FIELD_RETRY_AFTER,
    HTTP_FIELD_TRANSFER_ENCODING,
    HTTP_FIELD_UPGRADE,
    HTTP_FIELD_USER_AGENT,
    HTTP_FIELD_WWW_AUTHENTICATE,
    HTTP_FIELD_MAX
   );

   Phttp_t=^http_t;
   http_t=record
            fd : longint;            //File descriptor for this socket
            blocking : longint;      //To block or not to block
            error : longint;         //Last error on read
            activity : time_t;       //Time since last read/write
            state : http_state_t;    //State of client
            status : http_status_t;  //Status of last request
            version : http_version_t;//Protocol version
            keep_alive : http_keepalive_t; //Keep-alive supported?
            hostaddr : sockaddr_in;        //Address of connected host
            hostname : array[0..(HTTP_MAX_HOST)-1] of char; //Name of connected host
            fields : array[0..Ord(HTTP_FIELD_MAX)-1] of array[0..(HTTP_MAX_VALUE)-1] of char; //Field values
            data : ^char;                                   //Pointer to data buffer
            data_encoding : http_encoding_t;                //Chunked or not
            data_remaining : longint;                       //Number of bytes left
            used : longint;                                 //Number of bytes used in buffer
            buffer : array[0..(HTTP_MAX_BUFFER)-1] of char; //Buffer for messages
            auth_type : longint;                            //Authentication in use
            md5_state : md5_state_t;                        //MD5 state
            nonce : array[0..(HTTP_MAX_VALUE)-1] of char;   //Nonce value
            nonce_count : longint;                          //Nonce count
            tls : pointer;                                  //TLS state information
            encryption : http_encryption_t;                 //Encryption requirements
            input_set : Pfd_set;                            //select() set for httpWait()
         end;
    TArrayChar32=array[0..32] of char;


//
//
//  cups_ppd types and consts
//
//
const
  //PPD size limits (defined in Adobe spec
  PPD_MAX_NAME = 41;    //Maximum size of name + 1 for nul
  PPD_MAX_TEXT = 81;    //Maximum size of text + 1 for nul
  PPD_MAX_LINE = 256;   //Maximum size of line + 1 for nul
  PPD_VERSION  = '4.3'; //Kept in sync with Adobe version number

Type
  //UI types ...
  ppd_ui_t=(
    PPD_UI_BOOLEAN,		//True or False option
    PPD_UI_PICKONE,		//Pick one from a list
    PPD_UI_PICKMANY		//Pick zero or more from a list
  );

  //Order dependency sections
  ppd_section_t=(
    PPD_ORDER_ANY,		//Option code can be anywhere in the file
    PPD_ORDER_DOCUMENT,		//... must be in the DocumentSetup section
    PPD_ORDER_EXIT,		//... must be sent prior to the document
    PPD_ORDER_JCL,		//... must be sent as a JCL command
    PPD_ORDER_PAGE,		//... must be in the PageSetup section
    PPD_ORDER_PROLOG		//... must be in the Prolog section
  );

  //Colorspaces
  ppd_cs_t=(
    PPD_CS_CMYK := -4,		//CMYK colorspace
    PPD_CS_CMY,			//CMY colorspace
    PPD_CS_GRAY := 1,		//Grayscale colorspace
    PPD_CS_RGB := 3,		//RGB colorspace
    PPD_CS_RGBK,		//RGBK (K = gray) colorspace
    PPD_CS_N			//DeviceN colorspace
   );

  //Status Codes
  ppd_status_t=(
    PPD_OK := 0,		//OK
    PPD_FILE_OPEN_ERROR,	//Unable to open PPD file
    PPD_NULL_FILE,		//NULL PPD file pointer
    PPD_ALLOC_ERROR,		//Memory allocation error
    PPD_MISSING_PPDADOBE4,	//Missing PPD-Adobe-4.x header
    PPD_MISSING_VALUE,		//Missing value string
    PPD_INTERNAL_ERROR,		//Internal error
    PPD_BAD_OPEN_GROUP,		//Bad OpenGroup
    PPD_NESTED_OPEN_GROUP,	//openGroup without a CloseGroup first
    PPD_BAD_OPEN_UI,		//Bad OpenUI/JCLOpenUI
    PPD_NESTED_OPEN_UI,		//OpenUI/JCLOpenUI without a CloseUI/JCLCloseUI first
    PPD_BAD_ORDER_DEPENDENCY,	//Bad OrderDependency
    PPD_BAD_UI_CONSTRAINTS,	//Bad UIConstraints
    PPD_MISSING_ASTERISK,	//Missing asterisk in column 0
    PPD_LINE_TOO_LONG,		//Line longer than 255 chars
    PPD_ILLEGAL_CHARACTER,	//Illegal control character
    PPD_ILLEGAL_MAIN_KEYWORD,	//Illegal main keyword string
    PPD_ILLEGAL_OPTION_KEYWORD,	//Illegal option keyword string
    PPD_ILLEGAL_TRANSLATION	//Illegal translation string
  );

  //PPD Attribute Structure
  Pppd_attr_t  =^ppd_attr_t;
  PPppd_attr_t =^Pppd_attr_t;
  ppd_attr_t = record
    name : array[0..(PPD_MAX_NAME)-1] of char;                //Name of attribute (cupsXYZ)
    spec : array[0..(PPD_MAX_NAME + PPD_MAX_TEXT)-1] of char; //Specifier string, if any
    value: Pchar;                                             //Value string
  end;

  //Option choices
  Pppd_choice_t=^ppd_choice_t;
  PPppd_choice_t=^Pppd_choice_t;
  PPPppd_choice_t=^PPppd_choice_t;
  ppd_choice_t = record
    marked : Char;                              //0 if not selected, 1 otherwise
    choice : Array[0..(PPD_MAX_NAME)-1] of char;//Computer-readable option name
    text   : Array[0..(PPD_MAX_TEXT)-1] of char;//Human-readable option name
    code   : Pchar;                             //Code to send for this option
    option : Pointer;                           //Pointer to parent option structure
  end;

  //Options
  Pppd_option_t=^ppd_option_t;
  ppd_option_t = record
    conflicted : char;                              //0 if no conflicts exist, 1 otherwise
    keyword    : array[0..(PPD_MAX_NAME)-1] of char;//Option keyword name ("PageSize", etc.)
    defchoice  : array[0..(PPD_MAX_NAME)-1] of char;//Default option choice
    text       : array[0..(PPD_MAX_TEXT)-1] of char;//Human-readable text
    ui         : ppd_ui_t;                          //Type of UI option
    section    : ppd_section_t;                     //Section for command
    order      : integer;                           //Order number
    num_choices: LongInt;                           //Number of option choices
    choices    : Pppd_choice_t;                     //Option choices
  end;

  //Groups
   {Group text strings are limited to 39 chars + nul in order to
    preserve binary compatibility and allow applications to get
    the group's keyword name. }
  Pppd_group_str=^ppd_group_str;
  ppd_group_str = record
    text          : array[0..(PPD_MAX_TEXT - PPD_MAX_NAME)-1] of char; //Human-readable group name
    name          : array[0..(PPD_MAX_NAME)-1] of char;                //Group name
    num_options   : longint;                                           //Number of options
    options       : Pppd_option_t;                                     //Options
    num_subgroups : longint;                                           //Number of sub-groups
    subgroups     : Pppd_group_str;                                    //Sub-groups (max depth = 1)
  end;
  ppd_group_t = ppd_group_str;
  Pppd_group_t= ^ppd_group_t;

  //Constraints
  Pppd_const_t=^ppd_const_t;
  ppd_const_t = record
    option1 : array[0..(PPD_MAX_NAME)-1] of char; //First keyword
    choice1 : array[0..(PPD_MAX_NAME)-1] of char; //First option/choice (blank for all)
    option2 : array[0..(PPD_MAX_NAME)-1] of char; //Second keyword
    choice2 : array[0..(PPD_MAX_NAME)-1] of char; //Second option/choice (blank for all)
  end;

  //Page Sizes
  Pppd_size_t=^ppd_size_t;
  ppd_size_t = record
    marked : LongInt;                            //Page size selected?
    name   : array[0..(PPD_MAX_NAME)-1] of char; //Media size option
    width  : single;                             //Width of media in points
    length : single;                             //Length of media in points
    left   : single;                             //Left printable margin in points
    bottom : single;                             //Bottom printable margin in points
    right  : single;                             //Right printable margin in points
    top    : single;                             //Top printable margin in points
  end;

  //Emulators
  Pppd_emul_t=^ppd_emul_t;
  ppd_emul_t = record
    name : array[0..(PPD_MAX_NAME)-1] of char; //Emulator name
    start: Pchar;                              //Code to switch to this emulation
    stop : Pchar;                              //Code to stop this emulation
  end;

  //sRGB Color Profiles
  Pppd_profile_t=^ppd_profile_t;
  ppd_profile_t = record
    resolution : array[0..(PPD_MAX_NAME)-1] of char;   //Resolution or "-"
    media_type : array[0..(PPD_MAX_NAME)-1] of char;   //Media type of "-"
    density    : single;                               //Ink density to use
    gamma      : single;                               //Gamma correction to use
    matrix     : array[0..2] of array[0..2] of single; //Transform matrix
  end;

  //Files
  Pppd_file_t=^ppd_file_t;
  ppd_file_t = record
    language_level     : longint;                //Language level of device
    color_device       : longint;                //1=color device, 0=grayscale
    variable_sizes     : longint;                //1 = supports variable sizes, 0 = doesn't
    accurate_screens   : longint;                //1 = supports accurate screens, 0 = not
    contone_only       : longint;                //1 = continuous tone only, 0 = not
    landscape          : longint;                //-90 or 90
    model_number       : longint;                //Device-specific model number
    manual_copies      : longint;                //1 = Copies done manually, 0 = hardware
    throughput         : longint;                //Pages per minute
    colorspace         : ppd_cs_t;               //Default colorspace
    patches            : Pchar;                  //Patch commands to be sent to printer
    num_emulations     : longint;                //Number of emulations supported
    emulations         : Pppd_emul_t;            //Emulations and the code to invoke them
    jcl_begin          : Pchar;                  //Start JCL commands
    jcl_ps             : Pchar;                  //Enter PostScript interpreter
    jcl_end            : Pchar;                  //End JCL commands
    lang_encoding      : Pchar;                  //Language encoding
    lang_version       : Pchar;                  //Language version (English, Spanish, etc.)
    modelname          : Pchar;                  //Model name (general)
    ttrasterizer       : Pchar;                  //Truetype rasterizer
    manufacturer       : Pchar;                  //Manufacturer name
    product            : Pchar;                  //Product name (from PS RIP/interpreter)
    nickname           : Pchar;                  //Nickname (specific)
    shortnickname      : Pchar;                  //Short version of nickname
    num_groups         : longint;                //Number of UI groups
    groups             : Pppd_group_t;           //UI groups
    num_sizes          : longint;                //Number of page sizes
    sizes              : Pppd_size_t;            //Page sizes
    custom_min         : array[0..1] of single;  //Minimum variable page size
    custom_max         : array[0..1] of single;  //Maximum variable page size
    custom_margins     : array[0..3] of single;  //Margins around page
    num_consts         : longint;                //Number of UI/Non-UI constraints
    consts             : Pppd_const_t;           //UI/Non-UI constraints
    num_fonts          : longint;                //Number of pre-loaded fonts
    fonts              : PPchar;                 //Pre-loaded fonts
    num_profiles       : longint;                //Number of sRGB color profiles
    profiles           : Pppd_profile_t;         //sRGB color profiles
    num_filters        : longint;                //Number of filters
    filters            : PPchar;                 //Filter strings...
    flip_duplex        : longint;                //1 = Flip page for back sides (New in CUPS 1.1)
    protocols          : Pchar;                  //Protocols (BCP, TBCP) string (New in CUPS 1.1.19)
    pcfilename         : Pchar;                  //PCFileName string
    num_attrs          : longint;                //Number of attributes
    cur_attr           : longint;                //Current attribute
    attrs              : PPppd_attr_t;           //Attributes
  end;

//
//
//  cups_ipp types and consts
//
//
const
  IPP_VERSION =	#1#1;
   { IPP registered port number...  This is the default value - applications
    should use the ippPort() function so that you can customize things in
    /etc/services if needed!}
  IPP_PORT = 631;
  IPP_MAX_NAME = 256;  //Common limits...
  IPP_MAX_VALUES = 10; //Now just an allocation increment

Type
  //Job States....
  ipp_jstate_t = (IPP_JOB_PENDING := 3,
                  IPP_JOB_HELD,
                  IPP_JOB_PROCESSING,
                  IPP_JOB_STOPPED,
                  IPP_JOB_CANCELLED,
                  IPP_JOB_ABORTED,
                  IPP_JOB_COMPLETED);
  //Qualities...
  ipp_quality_t= (IPP_QUALITY_DRAFT := 3,
                  IPP_QUALITY_NORMAL,
                  IPP_QUALITY_HIGH);

  //Orientation...
  ipp_orient_t = (IPP_PORTRAIT := 3,      //No rotation
                  IPP_LANDSCAPE,          //90 degrees counter-clockwise
                  IPP_REVERSE_LANDSCAPE,  //90 degrees clockwise
                  IPP_REVERSE_PORTRAIT);  //180 degrees

  {Format tags for attribute formats...
   Mask for copied attribute values
   Bitflag for copied attribute values  }
  ipp_tag_t = (IPP_TAG_ZERO := $00,
               IPP_TAG_OPERATION,
               IPP_TAG_JOB,
               IPP_TAG_END,
               IPP_TAG_PRINTER,
               IPP_TAG_UNSUPPORTED_GROUP,
               IPP_TAG_SUBSCRIPTION,
               IPP_TAG_EVENT_NOTIFICATION,
               IPP_TAG_UNSUPPORTED_VALUE := $10,
               IPP_TAG_DEFAULT,
               IPP_TAG_UNKNOWN,
               IPP_TAG_NOVALUE,
               IPP_TAG_NOTSETTABLE := $15,
               IPP_TAG_DELETEATTR,
               IPP_TAG_ADMINDEFINE,
               IPP_TAG_INTEGER := $21,
               IPP_TAG_BOOLEAN,
               IPP_TAG_ENUM,
               IPP_TAG_STRING := $30,
               IPP_TAG_DATE,
               IPP_TAG_RESOLUTION,
               IPP_TAG_RANGE,
               IPP_TAG_BEGIN_COLLECTION,
               IPP_TAG_TEXTLANG,
               IPP_TAG_NAMELANG,
               IPP_TAG_END_COLLECTION,
               IPP_TAG_TEXT := $41,
               IPP_TAG_NAME,
               IPP_TAG_KEYWORD := $44,
               IPP_TAG_URI,
               IPP_TAG_URISCHEME,
               IPP_TAG_CHARSET,
               IPP_TAG_LANGUAGE,
               IPP_TAG_MIMETYPE,
               IPP_TAG_MEMBERNAME,
               IPP_TAG_MASK := $7fffffff,
               IPP_TAG_COPY := (-($7fffffff)) - 1);

  //Resolution units...
  ipp_res_t = (IPP_RES_PER_INCH := 3,
               IPP_RES_PER_CM);

  //Finishings...
  ipp_finish_t = (IPP_FINISHINGS_NONE := 3,
                  IPP_FINISHINGS_STAPLE,
                  IPP_FINISHINGS_PUNCH,
                  IPP_FINISHINGS_COVER,
                  IPP_FINISHINGS_BIND,
                  IPP_FINISHINGS_SADDLE_STITCH,
                  IPP_FINISHINGS_EDGE_STITCH,
                  IPP_FINISHINGS_FOLD,
                  IPP_FINISHINGS_TRIM,
                  IPP_FINISHINGS_BALE,
                  IPP_FINISHINGS_BOOKLET_MAKER,
                  IPP_FINISHINGS_JOB_OFFSET,
                  IPP_FINISHINGS_STAPLE_TOP_LEFT := 20,
                  IPP_FINISHINGS_STAPLE_BOTTOM_LEFT,
                  IPP_FINISHINGS_STAPLE_TOP_RIGHT,
                  IPP_FINISHINGS_STAPLE_BOTTOM_RIGHT,
                  IPP_FINISHINGS_EDGE_STITCH_LEFT,
                  IPP_FINISHINGS_EDGE_STITCH_TOP,
                  IPP_FINISHINGS_EDGE_STITCH_RIGHT,
                  IPP_FINISHINGS_EDGE_STITCH_BOTTOM,
                  IPP_FINISHINGS_STAPLE_DUAL_LEFT,
                  IPP_FINISHINGS_STAPLE_DUAL_TOP,
                  IPP_FINISHINGS_STAPLE_DUAL_RIGHT,
                  IPP_FINISHINGS_STAPLE_DUAL_BOTTOM,
                  IPP_FINISHINGS_BIND_LEFT := 50,
                  IPP_FINISHINGS_BIND_TOP,
                  IPP_FINISHINGS_BIND_RIGHT,
                  IPP_FINISHINGS_BIND_BOTTOM
                 );

  ipp_status_t = (IPP_OK := $0000,
                  IPP_OK_SUBST,
                  IPP_OK_CONFLICT,
                  IPP_OK_IGNORED_SUBSCRIPTIONS,
                  IPP_OK_IGNORED_NOTIFICATIONS,
                  IPP_OK_TOO_MANY_EVENTS,
                  IPP_OK_BUT_CANCEL_SUBSCRIPTION,
                  IPP_REDIRECTION_OTHER_SITE := $300,
                  IPP_BAD_REQUEST := $0400,
                  IPP_FORBIDDEN,
                  IPP_NOT_AUTHENTICATED,
                  IPP_NOT_AUTHORIZED,
                  IPP_NOT_POSSIBLE,
                  IPP_TIMEOUT,
                  IPP_NOT_FOUND,
                  IPP_GONE,
                  IPP_REQUEST_ENTITY,
                  IPP_REQUEST_VALUE,
                  IPP_DOCUMENT_FORMAT,
                  IPP_ATTRIBUTES,
                  IPP_URI_SCHEME,
                  IPP_CHARSET,IPP_CONFLICT,
                  IPP_COMPRESSION_NOT_SUPPORTED,
                  IPP_COMPRESSION_ERROR,
                  IPP_DOCUMENT_FORMAT_ERROR,
                  IPP_DOCUMENT_ACCESS_ERROR,
                  IPP_ATTRIBUTES_NOT_SETTABLE,
                  IPP_IGNORED_ALL_SUBSCRIPTIONS,
                  IPP_TOO_MANY_SUBSCRIPTIONS,
                  IPP_IGNORED_ALL_NOTIFICATIONS,
                  IPP_PRINT_SUPPORT_FILE_NOT_FOUND,
                  IPP_INTERNAL_ERROR := $0500,
                  IPP_OPERATION_NOT_SUPPORTED,
                  IPP_SERVICE_UNAVAILABLE,
                  IPP_VERSION_NOT_SUPPORTED,
                  IPP_DEVICE_ERROR,
                  IPP_TEMPORARY_ERROR,
                  IPP_NOT_ACCEPTING,
                  IPP_PRINTER_BUSY,
                  IPP_ERROR_JOB_CANCELLED,
                  IPP_MULTIPLE_JOBS_NOT_SUPPORTED,
                  IPP_PRINTER_IS_DEACTIVATED);

  //Printer States....
  ipp_pstate_t = (IPP_PRINTER_IDLE := 3,
                  IPP_PRINTER_PROCESSING,
                  IPP_PRINTER_STOPPED);

  //IPP states...
  ipp_state_t = (IPP_ERROR := -(1), //An error occurred
                 IPP_IDLE,          //Nothing is happening/request completed
                 IPP_HEADER,        //The request header needs to be sent/received
                 IPP_ATTRIBUTE,     //One or more attributes need to be sent/received
                 IPP_DATA);         //IPP request data needs to be sent/received

   //IPP operations...
  ipp_op_t = (IPP_PRINT_JOB := $0002,
              IPP_PRINT_URI,
              IPP_VALIDATE_JOB,
              IPP_CREATE_JOB,
              IPP_SEND_DOCUMENT,
              IPP_SEND_URI,
              IPP_CANCEL_JOB,
              IPP_GET_JOB_ATTRIBUTES,
              IPP_GET_JOBS,
              IPP_GET_PRINTER_ATTRIBUTES,
              IPP_HOLD_JOB,
              IPP_RELEASE_JOB,
              IPP_RESTART_JOB,
              IPP_PAUSE_PRINTER := $0010,
              IPP_RESUME_PRINTER,
              IPP_PURGE_JOBS,
              IPP_SET_PRINTER_ATTRIBUTES,
              IPP_SET_JOB_ATTRIBUTES,
              IPP_GET_PRINTER_SUPPORTED_VALUES,
              IPP_CREATE_PRINTER_SUBSCRIPTION,
              IPP_CREATE_JOB_SUBSCRIPTION,
              IPP_GET_SUBSCRIPTION_ATTRIBUTES,
              IPP_GET_SUBSCRIPTIONS,
              IPP_RENEW_SUBSCRIPTION,
              IPP_CANCEL_SUBSCRIPTION,
              IPP_GET_NOTIFICATIONS,
              IPP_SEND_NOTIFICATIONS,
              IPP_GET_PRINT_SUPPORT_FILES := $0021,
              IPP_ENABLE_PRINTER,
              IPP_DISABLE_PRINTER,
              IPP_PAUSE_PRINTER_AFTER_CURRENT_JOB,
              IPP_HOLD_NEW_JOBS,
              IPP_RELEASE_HELD_NEW_JOBS,
              IPP_DEACTIVATE_PRINTER,
              IPP_ACTIVATE_PRINTER,
              IPP_RESTART_PRINTER,
              IPP_SHUTDOWN_PRINTER,
              IPP_STARTUP_PRINTER,
              IPP_REPROCESS_JOB,
              IPP_CANCEL_CURRENT_JOB,
              IPP_SUSPEND_CURRENT_JOB,
              IPP_RESUME_JOB,
              IPP_PROMOTE_JOB,
              IPP_SCHEDULE_JOB_AFTER,
              IPP_PRIVATE := $4000,
              CUPS_GET_DEFAULT,
              CUPS_GET_PRINTERS,
              CUPS_ADD_PRINTER,
              CUPS_DELETE_PRINTER,
              CUPS_GET_CLASSES,
              CUPS_ADD_CLASS,
              CUPS_DELETE_CLASS,
              CUPS_ACCEPT_JOBS,
              CUPS_REJECT_JOBS,
              CUPS_SET_DEFAULT,
              CUPS_GET_DEVICES,
              CUPS_GET_PPDS,
              CUPS_MOVE_JOB,
              CUPS_ADD_DEVICE,
              CUPS_DELETE_DEVICE
             );

  Pipp_uchar_t=^ipp_uchar_t;
  ipp_uchar_t = byte;   //Unsigned 8-bit integer/character

  //Request Header
  ipp_request_t = record
      case longint of
         0 : (any : record  //Any Header
                      version : array[0..1] of ipp_uchar_t; //Protocol version number
                      op_status : longint;                  //Operation ID or status code
                      request_id : longint;                 //Request ID
                    end);
         1 : (op : record  //Operation ID
                     version : array[0..1] of ipp_uchar_t;  //Protocol version number
                     operation_id : ipp_op_t;               //Operation ID
                     request_id : longint;                  //Request ID
                   end);
         2 : (status : record //Status Header
                         version:array[0..1] of ipp_uchar_t;//Protocol version number
                         status_code : ipp_status_t;        //Status code
                         request_id : longint;              //Request ID
                       end);
      end;


  //Attribute Value
  ipp_value_t = record
      case longint of
        0 : ( aInteger : longint );                  //Integer/enumerated value
        1 : ( aBoolean : char );                     //Boolean value
        2 : ( aDate : array[0..10] of ipp_uchar_t ); //Date/time value
        3 : ( resolution : record                    //Resolution value
                             xres : longint;           //Horizontal resolution
                             yres : longint;           //Vertical resolution
                             units : ipp_res_t;        //Resolution units
                            end );
        4 : ( range : record                         //Range of integers value
                        lower : longint;               //Lower value
                        upper : longint;               //Upper value
                      end );
        5 : ( _string : record                       //String with language value
                          charset: Pchar;              //Character set
                          text   : Pchar;              //String
                        end );
        6 : ( unknown : record                       //Unknown attribute type
                          length : longint;            //Length of attribute
                          data : pointer;              //Data in attribute
                        end );
     end;
  Pipp_value_t = ^ipp_value_t;


  //Attribute
  Pipp_attribute_s=^ipp_attribute_s;
  ipp_attribute_s = record
         next      : Pipp_attribute_s;   //Next attribute in list
         group_tag : ipp_tag_t;          //Job/Printer/Operation group tag
         value_tag : ipp_tag_t;          //What type of value is it?
         name      : Pchar;              //Name of attribute
         num_values: longint;            //Number of values
         values    : array[0..0] of ipp_value_t; //Values
  end;
  ipp_attribute_t = ipp_attribute_s;
  Pipp_attribute_t=^ipp_attribute_t;

  //Request State
  Pipp_t=^ipp_t;
  ipp_t = record
            state   : ipp_state_t;      //State of request
            request : ipp_request_t;    //Request header
            attrs   : Pipp_attribute_t; //Attributes
            last    : Pipp_attribute_t; //Last attribute in list
            current : Pipp_attribute_t; //Current attribute (for read/write)
            curtag  : ipp_tag_t;        //Current attribute group tag
          end;

//
//
//
//
const
  MaxcupsLibs=2;
  cupsLibs :Array[0..MaxcupsLibs] of string = ('libcups.so',
                                               'libcups.so.2',
                                               '/usr/lib/libcups.dylib');

const
  CUPS_VERSION = 1.0119;
  CUPS_VERSION_MAJOR = 1;
  CUPS_VERSION_MINOR = 1;
  CUPS_VERSION_PATCH = 19;
  CUPS_DATE_ANY = -(1);

  //**** Printer Type/Capability Bits ********
  //not a typedef'd enum so we can OR
  CUPS_PRINTER_LOCAL     = $0000;	// Local printer or class
  CUPS_PRINTER_CLASS     = $0001;	// Printer class
  CUPS_PRINTER_REMOTE    = $0002;	// Remote printer or class
  CUPS_PRINTER_BW        = $0004;	// Can do B&W printing
  CUPS_PRINTER_COLOR     = $0008;	// Can do color printing
  CUPS_PRINTER_DUPLEX    = $0010;	// Can do duplexing
  CUPS_PRINTER_STAPLE    = $0020;	// Can staple output
  CUPS_PRINTER_COPIES    = $0040;	// Can do copies
  CUPS_PRINTER_COLLATE   = $0080;	// Can collage copies
  CUPS_PRINTER_PUNCH     = $0100;	// Can punch output
  CUPS_PRINTER_COVER     = $0200;	// Can cover output
  CUPS_PRINTER_BIND      = $0400;	// Can bind output
  CUPS_PRINTER_SORT      = $0800;	// Can sort output
  CUPS_PRINTER_SMALL     = $1000;	// Can do Letter/Legal/A4
  CUPS_PRINTER_MEDIUM    = $2000;	// Can do Tabloid/B/C/A3/A2
  CUPS_PRINTER_LARGE     = $4000;	// Can do D/E/A1/A0
  CUPS_PRINTER_VARIABLE  = $8000;	// Can do variable sizes
  CUPS_PRINTER_IMPLICIT  = $10000;	// Implicit class
  CUPS_PRINTER_DEFAULT   = $20000;	// Default printer on network
  CUPS_PRINTER_OPTIONS   = $FFFC;	// ~(CLASS | REMOTE | IMPLICIT)

type
  PPPChar = ^PPChar;
  cups_ptype_t = dword; //See bit flags consts
  TFunctionWithParam1 = function (_para1:Pchar):Pchar;

  //Printer Options
  Pcups_option_t= ^cups_option_t;
  PPcups_option_t=^Pcups_option_t;
  cups_option_t = record
                    name  : PChar; //Name of option
                    value : PChar; //Value of option
                  end;

  //Destination
  Pcups_dest_t=^cups_dest_t;
  PPcups_dest_t=^Pcups_dest_t;
  cups_dest_t = record
                  name       : PChar;          //Printer or class name
                  instance   : PChar;          //Local instance name or NULL
                  is_default : Longint;        //Is this printer the default?
                  num_options: Longint;        //Number of options
                  options    : Pcups_option_t; //Options
                end;
  //Job
  Pcups_job_t=^cups_job_t;
  PPcups_job_t=^Pcups_job_t;
  cups_job_t = record
                 id               : longint;       //The job ID
                 dest             : Pchar;         //Printer or class name
                 title            : Pchar;         //Title/job name
                 user             : Pchar;         //User the submitted the job
                 format           : Pchar;         //Document format
                 state            : ipp_jstate_t;  //Job state
                 size             : longint;       //Size in kilobytes
                 priority         : longint;       //Priority (1-100)
                 completed_time   : time_t;        //Time the job was completed
                 creation_time    : time_t;        //Time the job was created
                 processing_time  : time_t;        //Time the job was processed
               end;


var
  //
  //cups_language.pp
  //
  cupsLangEncoding: function (lang:Pcups_lang_t):Pchar;cdecl;
  cupsLangFlush: procedure; cdecl;
  cupsLangFree: procedure (lang:Pcups_lang_t);cdecl;
  cupsLangGet: function (language:Pchar):Pcups_lang_t;cdecl;
  //
  //cups_http.pp
  //
  httpCheck: function (http:Phttp_t):longint;cdecl;
  httpClose: procedure (http_t :Phttp_t); cdecl;
  httpConnect: function (host:Pchar; port:longint):Phttp_t;cdecl;
  httpConnectEncrypt: function (host:Pchar; port:longint; encrypt:http_encryption_t):Phttp_t;cdecl;
  httpDelete: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpEncryption: function (http:Phttp_t; e:http_encryption_t):longint;cdecl;
  httpError: function (http : longint) : longint; cdecl;
  httpFlush: procedure (http:Phttp_t);cdecl;
  httpGet: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpGets: function (line:Pchar; length:longint; http:Phttp_t):Pchar;cdecl;
  httpGetDateString: function (t:time_t):Pchar;cdecl;
  httpGetDateTime: function (s:Pchar):time_t;cdecl;
  httpGetField: function (http,field : longint) : longint; cdecl;
  //httpGetHostByName: function (name:Pchar):^hostent;cdecl;
  httpGetSubField: function (http:Phttp_t; field:http_field_t; name:Pchar; value:Pchar):Pchar;cdecl;
  httpHead: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpInitialize: procedure ;cdecl;
  httpOptions: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpPost: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpPut: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpRead: function (http:Phttp_t; buffer:Pchar; length:longint):longint;cdecl;
  httpReconnect: function (http:Phttp_t):longint;cdecl;
  httpSeparate: procedure (uri:Pchar; method:Pchar; username:Pchar; host:Pchar; port:Plongint; resource:Pchar);cdecl;
  httpSetField: procedure (http:Phttp_t; field:http_field_t; value:Pchar);cdecl;
  httpStatus: function (status:http_status_t):Pchar;cdecl;
  httpTrace: function (http:Phttp_t; uri:Pchar):longint;cdecl;
  httpUpdate: function (http:Phttp_t):http_status_t;cdecl;
  httpWait: function (http:Phttp_t; msec:longint):longint;cdecl;
  httpWrite: function (http:Phttp_t; buffer:Pchar; length:longint):longint;cdecl;
  //httpEncode64: function (out:Pchar; in:Pchar):Pchar;cdecl;
  //httpDecode64: function (out:Pchar; in:Pchar):Pchar;cdecl;
  httpGetLength: function (http:Phttp_t):longint;cdecl;
  httpMD5: function (_para1:Pchar; _para2:Pchar; _para3:Pchar; _para4:TArrayChar32):Pchar;cdecl;
  httpMD5Final: function (_para1:Pchar; _para2:Pchar; _para3:Pchar; _para4:TArrayChar32):Pchar;cdecl;
  httpMD5String: function (_para1:Pmd5_byte_t; _para2:TArrayChar32):Pchar;cdecl;

  //
  // cups_ppd
  //
  ppdClose: procedure (ppd:Pppd_file_t);cdecl;
  ppdCollect: function (ppd:Pppd_file_t; section:ppd_section_t; choices:PPPppd_choice_t):longint;cdecl;
  ppdConflicts: function (ppd:Pppd_file_t):longint;cdecl;
  ppdEmit: function (ppd:Pppd_file_t; fp:PFILE; section:ppd_section_t):longint;cdecl;
  ppdEmitFd: function (ppd:Pppd_file_t; fd:longint; section:ppd_section_t):longint;cdecl;
  ppdEmitJCL: function (ppd:Pppd_file_t; fp:PFILE; job_id:longint; user:Pchar; title:Pchar):longint;cdecl;
  ppdFindChoice: function (o:Pppd_option_t; option:Pchar):Pppd_choice_t;cdecl;
  ppdFindMarkedChoice: function (ppd:Pppd_file_t; keyword:Pchar):Pppd_choice_t;cdecl;
  ppdFindOption: function (ppd:Pppd_file_t; keyword:Pchar):Pppd_option_t;cdecl;
  ppdIsMarked: function (ppd:Pppd_file_t; keyword:Pchar; option:Pchar):longint;cdecl;
  ppdMarkDefaults: procedure (ppd:Pppd_file_t);cdecl;
  ppdMarkOption: function (ppd:Pppd_file_t; keyword:Pchar; option:Pchar):longint;cdecl;
  ppdOpen: function (fp:PFILE):Pppd_file_t;cdecl;
  ppdOpenFd: function (fd:longint):Pppd_file_t;cdecl;
  ppdOpenFile: function (filename:Pchar):Pppd_file_t;cdecl;
  ppdPageLength: function (ppd:Pppd_file_t; name:Pchar):single;cdecl;
  ppdPageSize: function (ppd:Pppd_file_t; name:Pchar):Pppd_size_t;cdecl;
  ppdPageWidth: function (ppd:Pppd_file_t; name:Pchar):single;cdecl;
  {New in CUPS 1.1.19}
  ppdErrorString: function (status:ppd_status_t):Pchar;cdecl;
  ppdFindAttr: function (ppd:Pppd_file_t; name:Pchar; spec:Pchar):Pchar;cdecl;
  ppdFindNextAttr: function (ppd:Pppd_file_t; name:Pchar; spec:Pchar):Pchar;cdecl;
  ppdLastError: function (line:Plongint):ppd_status_t;cdecl;

  //
  // cups_ipp
  //
  ippAddBoolean: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; value:char):Pipp_attribute_t;cdecl;
  ippAddBooleans: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; num_values:longint; values:Pchar):Pipp_attribute_t;cdecl;
  ippAddDate: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; value:Pipp_uchar_t):Pipp_attribute_t;cdecl;
  ippAddInteger: function (ipp:Pipp_t; group:ipp_tag_t; _type:ipp_tag_t; name:Pchar; value:longint):Pipp_attribute_t;cdecl;
  ippAddIntegers: function (ipp:Pipp_t; group:ipp_tag_t; _type:ipp_tag_t; name:Pchar; num_values:longint;
               values:Plongint):Pipp_attribute_t;cdecl;
  ippAddRange: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; lower:longint; upper:longint):Pipp_attribute_t;cdecl;
  ippAddRanges: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; num_values:longint; lower:Plongint;
               upper:Plongint):Pipp_attribute_t;cdecl;
  ippAddResolution: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; units:ipp_res_t; xres:longint;
               yres:longint):Pipp_attribute_t;cdecl;
  ippAddResolutions: function (ipp:Pipp_t; group:ipp_tag_t; name:Pchar; num_values:longint; units:ipp_res_t;
               xres:Plongint; yres:Plongint):Pipp_attribute_t;cdecl;
  ippAddSeparator: function (ipp:Pipp_t):Pipp_attribute_t;cdecl;
  ippAddString: function (ipp:Pipp_t; group:ipp_tag_t; _type:ipp_tag_t; name:Pchar; charset:Pchar;
               value:Pchar):Pipp_attribute_t;cdecl;
  ippAddStrings: function (ipp:Pipp_t; group:ipp_tag_t; _type:ipp_tag_t; name:Pchar; num_values:longint;
               charset:Pchar; values:PPchar):Pipp_attribute_t;cdecl;
  ippDateToTime: function (date:Pipp_uchar_t):time_t;cdecl;
  ippDelete: procedure (ipp:Pipp_t);cdecl;
  ippErrorString: function (error:ipp_status_t):Pchar;cdecl;
  ippFindAttribute: function (ipp:Pipp_t; name:Pchar; _type:ipp_tag_t):Pipp_attribute_t;cdecl;
  ippFindNextAttribute: function (ipp:Pipp_t; name:Pchar; _type:ipp_tag_t):Pipp_attribute_t;cdecl;
  ippLength: function (ipp:Pipp_t):size_t;cdecl;
  ippNew: function :Pipp_t;cdecl;
  ippRead: function (http:Phttp_t; ipp:Pipp_t):ipp_state_t;cdecl;
  ippTimeToDate: function (t:time_t):Pipp_uchar_t;cdecl;
  ippWrite: function (http:Phttp_t; ipp:Pipp_t):ipp_state_t;cdecl;
  ippPort: function :longint;cdecl;
  ippSetPort: procedure (p:longint);cdecl;
  _ipp_add_attr: function (_para1:Pipp_t; _para2:longint):Pipp_attribute_t;cdecl;
  _ipp_free_attr: procedure (_para1:Pipp_attribute_t);cdecl;

  //
  //cups.pp
  //
  cupsServer: function :Pchar;cdecl;
  cupsGetDefault:function :PChar; cdecl;
  cupsGetPPD:function (printer :Pchar): PChar;cdecl;
  cupsLastError:function : ipp_status_t; cdecl;
  cupsGetPrinters:function (printers :PPPchar):longint;cdecl;
  cupsDoFileRequest:function (http:Phttp_t; request:Pipp_t; const resource:Pchar; const filename:Pchar):Pipp_t;cdecl;
  cupsCancelJob:function (printer:Pchar; job:longint):longint;cdecl;
  cupsEncryption:function :http_encryption_t;cdecl;
  cupsFreeJobs:procedure (num_jobs:longint; jobs:Pcups_job_t);cdecl;
  cupsGetClasses:function (classes:PPPchar):longint;cdecl;
  cupsGetJobs:function (jobs:PPcups_job_t; dest:Pchar; myjobs:longint; completed:longint):longint;cdecl;
  cupsPrintFile:function (printer:Pchar; filename:Pchar; title:Pchar;
      num_options:longint; options:Pcups_option_t):longint;cdecl;
  cupsPrintFiles:function (printer:Pchar; num_files:longint; files:PPchar;
      title:Pchar; num_options:longint; options:Pcups_option_t):longint;cdecl;
  cupsTempFile:function (filename:Pchar; len:longint):Pchar;cdecl;
  cupsTempFd:function (filename:Pchar; len:longint):longint;cdecl;
  cupsAddDest:function (name:Pchar; instance:Pchar; num_dests:longint;
      dests:PPcups_dest_t):longint;cdecl;
  cupsFreeDests:procedure (num_dests:longint; dests:Pcups_dest_t);cdecl;
  cupsGetDest:function (name:Pchar; instance:Pchar;
      num_dests:longint; dests:Pcups_dest_t):Pcups_dest_t;cdecl;
  cupsGetDests:function (dests:PPcups_dest_t):longint;cdecl;
  cupsSetDests:procedure (num_dests:longint; dests:Pcups_dest_t);cdecl;
  cupsAddOption:function (name:Pchar; value:Pchar; num_options:longint; options:PPcups_option_t):longint;cdecl;
  cupsEncodeOptions:procedure (ipp:Pipp_t; num_options:longint; options:Pcups_option_t);cdecl;
  cupsFreeOptions:procedure (num_options:longint; options:Pcups_option_t);cdecl;
  cupsGetOption:function (name:Pchar; num_options:longint; options:Pcups_option_t):Pchar;cdecl;
  cupsParseOptions:function (arg:Pchar; num_options:longint; options:PPcups_option_t):longint;cdecl;
  cupsMarkOptions:function (ppd:Pppd_file_t; num_options:longint; options:Pcups_option_t):longint;cdecl;
  cupsGetPassword:function (prompt:Pchar):Pchar;cdecl;
  cupsSetEncryption:procedure (e:http_encryption_t);cdecl;
  cupsSetPasswordCB:procedure (cb: TFunctionWithParam1);cdecl;
  cupsSetServer:procedure (server:Pchar);cdecl;
  cupsSetUser:procedure (user:Pchar);cdecl;
  cupsUser:function :Pchar;cdecl;
  
function cupsDoRequest(ahttp :Phttp_t; aRequest :Pipp_t; aResource :PChar) : Pipp_t;
function cupsLangDefault : Pcups_lang_t;

procedure InitializeCups;
procedure FinalizeCups;

function CUPSLibInstalled : Boolean;

var
  CupsLibHandle: TLibHandle;
  
implementation

var RefCount : integer;

procedure InitializeCups;
var i : integer;
begin
  inc(RefCount);
  //debugln('InitializeCups RefCount=',dbgs(RefCount));
  if RefCount = 1 then
  begin
    for i:=0 to MaxcupsLibs do
    begin
      CupsLibHandle := loadlibrary(cupslibs[i]);
      if CupsLibHandle <> nilhandle then
        Break;
    end;
    
    if CupsLibHandle = nilhandle then
    begin
      debugln('InitializeCups load cups lib failed');
      RefCount := 0;
      raise EInOutError.Create('Can not load cups library');
    end;
  end;
  
  //WriteLn('CupsLibHandle=', CupsLibHandle);
  
  //
  //cups_language.pp
  //
  pointer(cupsLangEncoding) := GetProcedureAddress(CupsLibHandle, 'cupsLangEncoding');
  pointer(cupsLangFlush) := GetProcedureAddress(CupsLibHandle, 'cupsLangFlush');
  pointer(cupsLangFree) := GetProcedureAddress(CupsLibHandle, 'cupsLangFree');
  pointer(cupsLangGet) := GetProcedureAddress(CupsLibHandle, 'cupsLangGet');

  //
  // cups_http.pp
  //
  pointer(httpCheck) := GetProcedureAddress(CupsLibHandle, 'httpCheck');
  pointer(httpClose) := GetProcedureAddress(CupsLibHandle, 'httpClose');
  pointer(httpConnect) := GetProcedureAddress(CupsLibHandle, 'httpConnect');
  pointer(httpConnectEncrypt) := GetProcedureAddress(CupsLibHandle, 'httpConnectEncrypt');
  pointer(httpDelete) := GetProcedureAddress(CupsLibHandle, 'httpDelete');
  pointer(httpEncryption) := GetProcedureAddress(CupsLibHandle, 'httpEncryption');
  pointer(httpError) := GetProcedureAddress(CupsLibHandle, 'httpError');
  pointer(httpFlush) := GetProcedureAddress(CupsLibHandle, 'httpFlush');
  pointer(httpGet) := GetProcedureAddress(CupsLibHandle, 'httpGet');
  pointer(httpGets) := GetProcedureAddress(CupsLibHandle, 'httpGets');
  pointer(httpGetDateString) := GetProcedureAddress(CupsLibHandle, 'httpGetDateString');
  pointer(httpGetDateTime) := GetProcedureAddress(CupsLibHandle, 'httpGetDateTime');
  pointer(httpGetField) := GetProcedureAddress(CupsLibHandle, 'httpGetField');
  //pointer(httpGetHostByName) := GetProcedureAddress(CupsLibHandle, 'httpGetHostByName');
  pointer(httpGetSubField) := GetProcedureAddress(CupsLibHandle, 'httpGetSubField');
  pointer(httpHead) := GetProcedureAddress(CupsLibHandle, 'httpHead');
  pointer(httpInitialize) := GetProcedureAddress(CupsLibHandle, 'httpInitialize');
  pointer(httpOptions) := GetProcedureAddress(CupsLibHandle, 'httpOptions');
  pointer(httpPost) := GetProcedureAddress(CupsLibHandle, 'httpPost');
  pointer(httpPut) := GetProcedureAddress(CupsLibHandle, 'httpPut');
  pointer(httpRead) := GetProcedureAddress(CupsLibHandle, 'httpRead');
  pointer(httpReconnect) := GetProcedureAddress(CupsLibHandle, 'httpReconnect');
  pointer(httpSeparate) := GetProcedureAddress(CupsLibHandle, 'httpSeparate');
  pointer(httpSetField) := GetProcedureAddress(CupsLibHandle, 'httpSetField');
  pointer(httpStatus) := GetProcedureAddress(CupsLibHandle, 'httpStatus');
  pointer(httpTrace) := GetProcedureAddress(CupsLibHandle, 'httpTrace');
  pointer(httpUpdate) := GetProcedureAddress(CupsLibHandle, 'httpUpdate');
  pointer(httpWait) := GetProcedureAddress(CupsLibHandle, 'httpWait');
  pointer(httpWrite) := GetProcedureAddress(CupsLibHandle, 'httpWrite');
  //pointer(httpEncode64) := GetProcedureAddress(CupsLibHandle, 'httpEncode64');
  //pointer(httpDecode64) := GetProcedureAddress(CupsLibHandle, 'httpDecode64');
  pointer(httpGetLength) := GetProcedureAddress(CupsLibHandle, 'httpGetLength');
  pointer(httpMD5) := GetProcedureAddress(CupsLibHandle, 'httpMD5');
  pointer(httpMD5Final) := GetProcedureAddress(CupsLibHandle, 'httpMD5Final');
  pointer(httpMD5String) := GetProcedureAddress(CupsLibHandle, 'httpMD5String');

  //
  // cups_ppd
  //
  pointer(ppdClose) := GetProcedureAddress(CupsLibHandle, 'ppdClose');
  pointer(ppdCollect) := GetProcedureAddress(CupsLibHandle, 'ppdCollect');
  pointer(ppdConflicts) := GetProcedureAddress(CupsLibHandle, 'ppdConflicts');
  pointer(ppdEmit) := GetProcedureAddress(CupsLibHandle, 'ppdEmit');
  pointer(ppdEmitFd) := GetProcedureAddress(CupsLibHandle, 'ppdEmitFd');
  pointer(ppdEmitJCL) := GetProcedureAddress(CupsLibHandle, 'ppdEmitJCL');
  pointer(ppdFindChoice) := GetProcedureAddress(CupsLibHandle, 'ppdFindChoice');
  pointer(ppdFindMarkedChoice) := GetProcedureAddress(CupsLibHandle, 'ppdFindMarkedChoice');
  pointer(ppdFindOption) := GetProcedureAddress(CupsLibHandle, 'ppdFindOption');
  pointer(ppdIsMarked) := GetProcedureAddress(CupsLibHandle, 'ppdIsMarked');
  pointer(ppdMarkDefaults) := GetProcedureAddress(CupsLibHandle, 'ppdMarkDefaults');
  pointer(ppdMarkOption) := GetProcedureAddress(CupsLibHandle, 'ppdMarkOption');
  pointer(ppdOpen) := GetProcedureAddress(CupsLibHandle, 'ppdOpen');
  pointer(ppdOpenFd) := GetProcedureAddress(CupsLibHandle, 'ppdOpenFd');
  pointer(ppdOpenFile) := GetProcedureAddress(CupsLibHandle, 'ppdOpenFile');
  pointer(ppdPageLength) := GetProcedureAddress(CupsLibHandle, 'ppdPageLength');
  pointer(ppdPageSize) := GetProcedureAddress(CupsLibHandle, 'ppdPageSize');
  pointer(ppdPageWidth) := GetProcedureAddress(CupsLibHandle, 'ppdPageWidth');
  {New in CUPS 1.1.19}
  pointer(ppdErrorString) := GetProcedureAddress(CupsLibHandle, 'ppdErrorString');
  pointer(ppdFindAttr) := GetProcedureAddress(CupsLibHandle, 'ppdFindAttr');
  pointer(ppdFindNextAttr) := GetProcedureAddress(CupsLibHandle, 'ppdFindNextAttr');
  pointer(ppdLastError) := GetProcedureAddress(CupsLibHandle, 'ppdLastError');
  
  //
  // cups_ipp
  //
  pointer(ippAddBoolean) := GetProcedureAddress(CupsLibHandle, 'ippAddBoolean');
  pointer(ippAddBooleans) := GetProcedureAddress(CupsLibHandle, 'ippAddBooleans');
  pointer(ippAddDate) := GetProcedureAddress(CupsLibHandle, 'ippAddDate');
  pointer(ippAddInteger) := GetProcedureAddress(CupsLibHandle, 'ippAddInteger');
  pointer(ippAddIntegers) := GetProcedureAddress(CupsLibHandle, 'ippAddIntegers');
  pointer(ippAddRange) := GetProcedureAddress(CupsLibHandle, 'ippAddRange');
  pointer(ippAddRanges) := GetProcedureAddress(CupsLibHandle, 'ippAddRanges');
  pointer(ippAddResolution) := GetProcedureAddress(CupsLibHandle, 'ippAddResolution');
  pointer(ippAddResolutions) := GetProcedureAddress(CupsLibHandle, 'ippAddResolutions');
  pointer(ippAddSeparator) := GetProcedureAddress(CupsLibHandle, 'ippAddSeparator');
  pointer(ippAddString) := GetProcedureAddress(CupsLibHandle, 'ippAddString');
  pointer(ippAddStrings) := GetProcedureAddress(CupsLibHandle, 'ippAddStrings');
  pointer(ippDateToTime) := GetProcedureAddress(CupsLibHandle, 'ippDateToTime');
  pointer(ippDelete) := GetProcedureAddress(CupsLibHandle, 'ippDelete');
  pointer(ippErrorString) := GetProcedureAddress(CupsLibHandle, 'ippErrorString');
  pointer(ippFindAttribute) := GetProcedureAddress(CupsLibHandle, 'ippFindAttribute');
  pointer(ippFindNextAttribute) := GetProcedureAddress(CupsLibHandle, 'ippFindNextAttribute');
  pointer(ippLength) := GetProcedureAddress(CupsLibHandle, 'ippLength');
  pointer(ippNew) := GetProcedureAddress(CupsLibHandle, 'ippNew');
  pointer(ippRead) := GetProcedureAddress(CupsLibHandle, 'ippRead');
  pointer(ippTimeToDate) := GetProcedureAddress(CupsLibHandle, 'ippTimeToDate');
  pointer(ippWrite) := GetProcedureAddress(CupsLibHandle, 'ippWrite');
  pointer(ippPort) := GetProcedureAddress(CupsLibHandle, 'ippPort');
  pointer(ippSetPort) := GetProcedureAddress(CupsLibHandle, 'ippSetPort');
  pointer(_ipp_add_attr) := GetProcedureAddress(CupsLibHandle, '_ipp_add_attr');
  pointer(_ipp_free_attr) := GetProcedureAddress(CupsLibHandle, '_ipp_free_attr');

  //
  // cups.pp
  //
  pointer(cupsServer) := GetProcedureAddress(CupsLibHandle, 'cupsServer');
  pointer(cupsGetDefault) := GetProcedureAddress(CupsLibHandle, 'cupsGetDefault');
  pointer(cupsGetPPD) := GetProcedureAddress(CupsLibHandle, 'cupsGetPPD');
  pointer(cupsLastError) := GetProcedureAddress(CupsLibHandle, 'cupsLastError');
  pointer(cupsGetPrinters) := GetProcedureAddress(CupsLibHandle, 'cupsGetPrinters');
  pointer(cupsDoFileRequest) := GetProcedureAddress(CupsLibHandle, 'cupsDoFileRequest');
  pointer(cupsCancelJob) := GetProcedureAddress(CupsLibHandle, 'cupsCancelJob');
  pointer(cupsEncryption) := GetProcedureAddress(CupsLibHandle, 'cupsEncryption');
  pointer(cupsFreeJobs) := GetProcedureAddress(CupsLibHandle, 'cupsFreeJobs');
  pointer(cupsGetClasses) := GetProcedureAddress(CupsLibHandle, 'cupsGetClasses');
  pointer(cupsGetJobs) := GetProcedureAddress(CupsLibHandle, 'cupsGetJobs');
  pointer(cupsPrintFile) := GetProcedureAddress(CupsLibHandle, 'cupsPrintFile');
  pointer(cupsPrintFiles) := GetProcedureAddress(CupsLibHandle, 'cupsPrintFiles');
  pointer(cupsTempFile) := GetProcedureAddress(CupsLibHandle, 'cupsTempFile');
  pointer(cupsTempFd) := GetProcedureAddress(CupsLibHandle, 'cupsTempFd');
  pointer(cupsAddDest) := GetProcedureAddress(CupsLibHandle, 'cupsAddDest');
  pointer(cupsFreeDests) := GetProcedureAddress(CupsLibHandle, 'cupsFreeDests');
  pointer(cupsGetDest) := GetProcedureAddress(CupsLibHandle, 'cupsGetDest');
  pointer(cupsGetDests) := GetProcedureAddress(CupsLibHandle, 'cupsGetDests');
  pointer(cupsSetDests) := GetProcedureAddress(CupsLibHandle, 'cupsSetDests');
  pointer(cupsAddOption) := GetProcedureAddress(CupsLibHandle, 'cupsAddOption');
  pointer(cupsEncodeOptions) := GetProcedureAddress(CupsLibHandle, 'cupsEncodeOptions');
  pointer(cupsFreeOptions) := GetProcedureAddress(CupsLibHandle, 'cupsFreeOptions');
  pointer(cupsGetOption) := GetProcedureAddress(CupsLibHandle, 'cupsGetOption');
  pointer(cupsParseOptions) := GetProcedureAddress(CupsLibHandle, 'cupsParseOptions');
  pointer(cupsMarkOptions) := GetProcedureAddress(CupsLibHandle, 'cupsMarkOptions');
  pointer(cupsGetPassword) := GetProcedureAddress(CupsLibHandle, 'cupsGetPassword');
  pointer(cupsSetEncryption) := GetProcedureAddress(CupsLibHandle, 'cupsSetEncryption');
  pointer(cupsSetPasswordCB) := GetProcedureAddress(CupsLibHandle, 'cupsSetPasswordCB');
  pointer(cupsSetServer) := GetProcedureAddress(CupsLibHandle, 'cupsSetServer');
  pointer(cupsSetUser) := GetProcedureAddress(CupsLibHandle, 'cupsSetUser');
  pointer(cupsUser) := GetProcedureAddress(CupsLibHandle, 'cupsUser');
  
end;

//Return true if CUPS lib can be loaded and
//initilized.
function CUPSLibInstalled : Boolean;
begin
  Result:=False;
  if RefCount=0 then begin
    Try
      //debugln('CUPSLibInstalled A');
      InitializeCups;
    Except
      exit;
    end;
  end;
  Result:=(RefCount>0);
end;

procedure FinalizeCups;
begin
  //debugln('* FinalizeCups');
  if RefCount>0 then
    Dec(RefCount);
    
  if RefCount=0 then
  begin
    if (CupsLibHandle<>NilHandle) and not UnloadLibrary(CupsLibHandle) then
      Inc(RefCount)
    else
      CupsLibHandle := NilHandle;
  end;
end;

function cupsLangDefault : Pcups_lang_t;
begin
 Result:=cupsLangGet('');
end;

function cupsDoRequest(ahttp :Phttp_t; aRequest :Pipp_t;
  aResource :PChar):Pipp_t;
begin
  Result:=cupsDoFileRequest(aHttp,aRequest,aResource,nil);
end;

INITIALIZATION
  CupsLibHandle:= NilHandle;
  
end.

