{******************************************************************}
{*         IPCONST.PAS - Miscellaneous String Constants           *}
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

unit IpConst;

interface

const
  IpCRLF = #13#10;

const
  ReadLineErr = 8001;

resourcestring

  { Print Preview form}
  rsIpHTMLPreviewPrintPreview = 'Print preview';
  rsIpHTMLPreviewPrint = 'Print';
  rsIpHTMLPreviewZoom = 'Zoom:';
  rsIpHTMLPreviewClose = 'Close';
  rsIpHTMLPreviewFitAll = 'Fit all';
  rsIpHTMLPreviewFitWidth = 'Width';
  rsIpHTMLPreviewFitHeight = 'Height';
  rsIpHTMLPreviewPage = 'Page:';
  rsIpHTMLPreviewOf = 'of';
  rsIpHTMLPreviewSelectPrinter = 'Select printer ...';

  { General IPRO Errors }
  SWinSockErr = 'WinSock Error (%d): %s, on API ''%s''';
  SRasErr = 'Ras Error (%d): on API ''%s''';
  SActiveErr = 'Cannot set property on an active socket';
  SAccessProcErr = 'Win32 error loading ''%s'' pointer: %s';
  SNoWinSock2Err = '%s requires WinSock 2, and this system only has WinSock 1';
  SNoSockErr = 'Socket not assigned';
  SNoStreamErr = 'Stream not assigned';
  SNoTimerErr = 'Not enough system timers available';
  SIndexErr = 'Index out of range';
  SSocksErr = 'SOCKS request refused - %d';
  SReadLineErr = 'Received line too long, exceeds MaxLineBuf';
  SNoMemoryStreamErr = 'No Memory Stream assigned';

  { Event log }
  SEventConnect = 'Connect: Loc: %s Rem: %s';
  SEventDisconnect = 'Close: Loc: %s Rem: %s';

  { Terminal Errors }
  SNotEnoughData = 'Not enough data in queue (%s bytes) to satisfy read request (%s bytes)';
  SRowColOOR = '%s: either row %d or col %d is out of range';
  SRowRowOOR = 'TIpTerminalArray.ScrollRows: either start row %d or end row %d is out of range';
  SInvItemSize = 'TIpTerminalArray.GetItemPtr: invalid item size';
  SLessZero = '%s: new col count %d is less than zero';
  SPosReqd = '%s: count must be positive';
  SRowOOR = '%s: row number is out of range';
  SInvScrollRow = 'TIpTerminalBuffer.SetScrollRegion: invalid row number(s)';
  SCountTooSmall = '%s: new count too small';

  { General WinSock Errors }
  SwsaEIntr  = 'Interrupted function call';
  SwsaEBadF  = 'Bad file descriptor';
  SwsaEAcces = 'Permission denied';
  SwsaEFault = 'Bad address';
  SwsaEInval = 'Invalid argument';
  SwsaEMFile = 'Too many open files';
  SwsaEWouldBlock = 'Resource temporarily unavailable';
  SwsaEInProgress = 'Operation now in progress';
  SwsaEAlReady = 'Operation already in progress';
  SwsaENotSock = 'Socket operation on non socket';
  SwsaEDestAddrReq = 'Destination address required';
  SwsaEMsgSize = 'Message too long';
  SwsaEPrototype = 'Protocol wrong type for socket';
  SwsaENoProtoOpt = 'Bad protocol option';
  SwsaEProtoNoSupport = 'Protocol not supported';
  SwsaESocktNoSupport = 'Socket type not supported';
  SwsaEOpNotSupp = 'Operation not supported';
  SwsaEPfNoSupport = 'Protocol family not supported';
  SwsaEAfNoSupport = 'Address family not supported by protocol family';
  SwsaEAddrInUse = 'Address already in use';
  SwsaEAddrNotAvail = 'Cannot assign requested address';
  SwsaENetDown = 'Network is down';
  SwsaENetUnreach = 'Network is unreachable';
  SwsaENetReset = 'Network dropped connection on reset';
  SwsaEConnAborted = 'Software caused connection abort';
  SwsaEConnReset = 'Connection reset by peer';
  SwsaENoBufs = 'No buffer space available';
  SwsaEIsConn = 'Socket is already connected';
  SwsaENotConn = 'Socket is not connected';
  SwsaEShutDown = 'Cannot send after socket shutdown';
  SwsaETooManyRefs = 'Too many references; cannot splice';
  SwsaETimedOut = 'Connection timed out';
  SwsaEConnRefused = 'Connection refused';
  SwsaELoop = 'Too many levels of symbolic links';
  SwsaENameTooLong = 'File name too long';
  SwsaEHostDown = 'Host is down';
  SwsaEHostUnreach = 'No route to host';
  SwsaENotEmpty = 'Directory not empty';
  SwsaEProcLim = 'Too many processes';
  SwsaEUsers = 'Too many users';
  SwsaEDQuot = 'Disk quota exceeded';
  SwsaEStale = 'Stale NFS file handle';
  SwsaERemote = 'Too many levels of remote in path';
  SwsaSysNotReady = 'Network subsystem is unavailable';
  SwsaVerNotSupported = 'WinSock DLL version not supported';
  SwsaNotInitialised = 'Successful WSAStartup not yet performed';
  SwsaEDiscOn = 'Graceful shutdown in progress';
  SwsaENoMore = 'No more data available';
  SwsaECancelled = 'Cancelled';
  SwsaEInvalidProcTable = 'Invalid procedure table from service provider';
  SwsaEInvalidProvider = 'Invalid service provider version number';
  SwsaEProviderFailedInit = 'Unable to initialize a service provider';
  SwsaSysCallFailure = 'System call failure';
  SwsaService_Not_Found = 'Service not found';
  SwsaType_Not_Found = 'Type not found';
  Swsa_E_No_More = 'No more data available';
  Swsa_E_Cancelled = 'Lookup cancelled';
  SwsaERefused = 'Refused';
  SwsaHost_Not_Found = 'Host not found';
  SwsaTry_Again = 'Non-authoritative host not found';
  SwsaNo_Recovery = 'This is a nonrecoverable error';
  SwsaNo_Data = 'Valid name, no data record of requested type';
  Swsa_Qos_Receivers = 'At least one Reserve has arrived';
  Swsa_Qos_Senders = 'At least one Path has arrived';
  Swsa_Qos_No_Senders = 'There are no senders';
  Swsa_Qos_No_Receivers = 'There are no receivers';
  Swsa_Qos_Request_Confirmed = 'Reserve has been confirmed';
  Swsa_Qos_Admission_Failure = 'Error due to lack of resources';
  Swsa_Qos_Policy_Failure = 'Rejected for administrative reasons - bad credentials';
  Swsa_Qos_Bad_Style = 'Unknown or conflicting style';
  Swsa_Qos_Bad_Object = 'Problem filterspec or provider specific buffer';
  Swsa_Qos_Traffic_Ctrl_Error = 'Problem with some part of the flowspec';
  Swsa_Qos_Generic_Error = 'General error';

  { HTML Errors}
  SHTMLNotContainer = 'Parent is not a container';
  SHTMLLineError = 'Error "%s" at line %d, position %d';
  SHTMLCharStackOverfl = 'Character stack overflow';
  SHTMLTokenStackOverfl = 'Token stack overflow';
  SHTMLEncNotSupported = ' encoding not supported';
  SHTMLInternal = 'Internal error';
  SHTMLNoDataProvider = 'No data provider assigned';
  SHTMLResUnavail = 'Resource unavailable:';
  SHTMLUnsupProtocol = 'Unsupported protocol in URL:%s';
  SHTMLExp = ' expected';
  SHTMLDashExp = '- expected';
  SHTMLInvType = 'Invalid type specified';
  SHTMLUnknownTok = 'Unknown token';
  SHTMLInvInt = 'Invalid integer constant';
  SHTMLInvAlign = 'Invalid alignment specified';
  SHTMLInvValType = 'Invalid value type specified';
  SHTMLInvShape = 'Invalid shape specified';
  SHTMLInvMethod = 'Invalid method specified';
  SHTMLInvDir = 'Invalid dir value specified';
  SHTMLInvColor = 'Invalid color constant:';
  SHTMLInvFrame = 'Invalid frame specified';
  SHTMLInvRule = 'Invalid rule specified';
  SHTMLInvScope = 'Invalid scope specified';
  SHTMLInvScroll = 'Invalid scrolling specified';
  SHTMLDefSubmitCaption = 'Submit';
  SHTMLDefResetCaption = 'Reset';
  SHTMLDefBrowseCaption = 'Browse ...';
  SHTMLInvPicture = 'Invalid picture returned';
  SHTMLNoGraphic = 'Picture object contains no graphic object';
  SHTMLInvGraphic = 'Invalid graphic returned';
  SHTMLNoGetImage = 'No OnGetImage event handler assigned';

  { RAS status messages }
  SRasOpenPort = 'Open port';
  SRasPortOpened = 'Port opened';
  SRasConnectDevice = 'Connect device';
  SRasDeviceConnected = 'Device connected';
  SRasAllDevicesConnected = 'All devices connected';
  SRasAuthenticate = 'Authenticate';
  SRasAuthNotify = 'Authenticate notify';
  SRasAuthRetry = 'Authenticate retry';
  SRasAuthCallback = 'Authenticate callback';
  SRasAuthChangePassword = 'Authenticate change password';
  SRasAuthProject = 'Authenticate project';
  SRasAuthLinkSpeed = 'Authenticate link speed';
  SRasAuthAck = 'Authenticate acknowledged';
  SRasReAuthenticate = 'Re-authenticate';
  SRasAuthenticated = 'Authenticated';
  SRasPrepareForCallback = 'Prepare for callback';
  SRasWaitForModemReset = 'Wait for modem reset';
  SRasWaitForCallback = 'Wait for callback';
  SRasProjected = 'Projected';
  SRasStartAuthentication = 'Start authentication';
  SRasCallbackComplete = 'Callback complete';
  SRasLogonNetwork = 'Logon network';
  SRasSubEntryConnected = 'Sub-entry connected';
  SRasSubEntryDisconnected = 'Sub-entry disconnected';
  SRasPaused = 'Paused';
  SRasInteractive = 'Interactive';
  SRasRetryAuthentication = 'Retry authentication';
  SRasCallbackSetByCaller = 'Callback set by caller';
  SRasPasswordExpired = 'Password expired';
  SRasConnected = 'Connected';
  SRasDisconnected = 'Disconnected';

  { Ansi Text Stream Errors and Messages }
  SNoSeekForRead = 'No seek for read';
  SNoSeekForWrite = 'No seek for write';
  SCannotWriteToStream = 'Cannot write to stream';
  SBadSeekOrigin = 'Invalid seek origin';
  SBadLineTerminator = 'Invalid line terminator';
  SBadLineLength = 'Invalid line length';
  SBadPath = 'Path does not exist';
  SStreamCreated = 'Successfully created ';
  SStreamCreateError = 'Stream create error ';
  SDestroying = 'Destroying';
  SAttemptingToRead = 'Attempting to read ';
  SAttemptingToWrite = 'Attempting to write ';
  SBytesFromStream = ' bytes from stream';
  SBytesToStream = ' bytes to stream';
  SBytesReadFromStream = ' bytes read from stream';
  SBytesWrittenToStream = ' bytes written to stream';
  SFileName = ' Filename: ';
  SRenamedDiskFileTo = 'Renamed disk file to ';
  SSeekingDiskFileTo = 'Seeking disk file to ';
  SWriteAfterRename = '***Write after rename';
  SOriginFromBegin = 'When origin is soFromBeginning, Offset must be >= 0';
  SOriginFromEnd = 'When origin is soFromEnd, Offset must be <= 0';
  SMemMapFilenameRequired = 'You must specify a file name for TIpMemMapStream';
  SMemMapMustBeClosed = 'The %s method requires the TIpMemMapStream instance to be closed';
  SMemMapMustBeOpen = 'The %s method requires the TIpMemMapStream instance to be opened';

  { Mime message class errors and messages }
  SBadOffset = 'Invalid stream offset';
  SNoBoundary = 'No Mime boundary';
  SListNotAssigned = 'List not assigned';
  SBinHexBadFormat = 'Invalid BinHex format';
  SBinHexColonExpected = '":" expected';
  SBinHexBadChar = 'Invalid BinHex character';
  SBinHexOddChar = 'One odd character';
  SBinHexBadHeaderCRC = 'Bad header CRC';
  SBinHexBadDataCRC = 'Bad header CRC';
  SBinHexLengthErr = 'Invalid data length';
  SBinHexResourceForkErr = 'Resource fork present';
  SUUEncodeCountErr  = 'Count <> Len or Count > 63';
  SLineLengthErr = 'Invalid line length for encoded text';
  SUnsupportedEncoding = 'Encoding method not supported';

  { ICMP errors and messages }
  SIpICMP_SUCCESS                  = 'Successful';
  SIpICMP_BUF_TOO_SMALL            = 'Buffer too small';
  SIpICMP_DEST_NET_UNREACHABLE     = 'Destination network unreachable';
  SIpICMP_DEST_HOST_UNREACHABLE    = 'Destination host unreachable';
  SIpICMP_DEST_PROT_UNREACHABLE    = 'Destination protocol unreachable';
  SIpICMP_DEST_PORT_UNREACHABLE    = 'Destination port unreachable';
  SIpICMP_NO_RESOURCES             = 'Destination does not have resources to complete';
  SIpICMP_BAD_OPTION               = 'Bad option';
  SIpICMP_HW_ERROR                 = 'Hardware error';
  SIpICMP_PACKET_TOO_BIG           = 'Packet too large';
  SIpICMP_REQ_TIMED_OUT            = 'Request timed out';
  SIpICMP_BAD_REQ                  = 'Bad request';
  SIpICMP_BAD_ROUTE                = 'Bad route';
  SIpICMP_TTL_EXPIRED_TRANSIT      = 'Time to live expired during transmit';
  SIpICMP_TTL_EXPIRED_REASSEM      = 'Time to live expired during reassembly';
  SIpICMP_PARAM_PROBLEM            = 'Parameter problem';
  SIpICMP_SOURCE_QUENCH            = 'Destination is busy';
  SIpICMP_OPTION_TOO_BIG           = 'Option too large';
  SIpICMP_BAD_DESTINATION          = 'Bad destination';
  SIpICMP_Unknown                  = 'Unknown status';

  SlogIcmpClass           = '[ICMP] ';
  SIcmpEcho               = 'Echo reply (Hop number: %d)'+ IpCRLF +
                            '             Status = %d' + IpCRLF +
                            '             RTTime = %d' + IpCRLF +
                            '             Ttl = %d' + IpCRLF +
                            '             Tos = %d' + IpCRLF +
                            '             IpFlags = %d';
  SIcmpEchoString         = 'Echo string: %s';
  SIcmpPingStart          = 'Pinging %s with %d bytes data';

  SIcmpTraceStart         = 'Trace to %s started';
  SIcmpTraceComplete      = 'Trace complete (%s), %d hops';
  SIcmpThreadExecute      = 'Thread %d executing (Hop number = %d)';
  SIcmpThreadTerminate    = 'Thread %d terminating (Hop number = %d)';

  { Internet Messaging errors and messages }
  { TIpSmtpClient }
  { State descriptions }
  SWrongStateErr     = 'Can not comply, wrong state';
  SNoRecipients      = 'No recipients specified';
  SInvalRespCode     = 'Invalid response code';
  SssNoOp            = 'No operation';
  SssConnect         = 'Connecting';
  SssEhlo            = 'Logging on with EHLO';
  SssHelo            = 'Logging on with HELO';
  SssMailFrom        = 'Sending sender''s info';
  SssRcptTo          = 'Sending MailTo info';
  SssRcptCc          = 'Sending CC info';
  SssRcptBcc         = 'Sending BCC info';
  SssData            = 'Sending Data';
  SssRSet            = 'Resetting server';
  SssSend            = 'ssSend';
  SssSoml            = 'ssSoml';
  SssSaml            = 'ssSaml';
  SssVerify          = 'Verifying';
  SssExpand          = 'Expanding';
  SssHelp            = 'Help';
  SssTurn            = 'ssTurn';
  SssQuit            = 'Quit';
  SssSendEnvelope    = 'Sending Envelope';
  SssSendMessage     = 'Sending Message';
  SssSpecial         = 'Sending special command';
  SssAuthLogin       = 'Requesting authentication';
  SssAuthUser        = 'Authenticating username';
  SssAuthPass        = 'Authenticating password';
  { Task descriptions }
  SstNoTask          = 'None';
  SstLogon           = 'Logging on';
  SstSendMail        = 'Sending mail';
  SstError           = 'An error has occurred during this task.';
  { SMTP response codes, used by the TIpSmtpClient.ResultMsg method }
  SSmtpResponse02 = 'Success, ';
  SSmtpResponse04 = 'Transient, ';
  SSmtpResponse05 = 'Persistent, ';
  SSmtpResponse10 = 'Other address status';
  SSmtpResponse11 = 'Bad destination mailbox address';
  SSmtpResponse12 = 'Bad destination system address';
  SSmtpResponse13 = 'Bad destination mailbox address syntax';
  SSmtpResponse14 = 'Destination mailbox address ambiguous';
  SSmtpResponse15 = 'Destination mailbox address valid';
  SSmtpResponse16 = 'Mailbox has moved';
  SSmtpResponse17 = 'Bad sender''s mailbox address syntax';
  SSmtpResponse18 = 'Bad sender''s system address';
  SSmtpResponse20 = 'Other or undefined mailbox status';
  SSmtpResponse21 = 'Mailbox disabled, not accepting messages';
  SSmtpResponse22 = 'Mailbox full';
  SSmtpResponse23 = 'Message length exceeds administrative limit.';
  SSmtpResponse24 = 'Mailing list expansion problem';
  SSmtpResponse30 = 'Other or undefined mail system status';
  SSmtpResponse31 = 'Mail system full';
  SSmtpResponse32 = 'System not accepting network messages';
  SSmtpResponse33 = 'System not capable of selected features';
  SSmtpResponse34 = 'Message too big for system';
  SSmtpResponse40 = 'Other or undefined network or routing status';
  SSmtpResponse41 = 'No answer from host';
  SSmtpResponse42 = 'Bad connection';
  SSmtpResponse43 = 'Routing server failure';
  SSmtpResponse44 = 'Unable to route';
  SSmtpResponse45 = 'Network congestion';
  SSmtpResponse46 = 'Routing loop detected';
  SSmtpResponse47 = 'Delivery time expired';
  SSmtpResponse50 = 'Other or undefined protocol status';
  SSmtpResponse51 = 'Invalid command';
  SSmtpResponse52 = 'Syntax error';
  SSmtpResponse53 = 'Too many recipients';
  SSmtpResponse54 = 'Invalid command arguments';
  SSmtpResponse55 = 'Wrong protocol version';
  SSmtpResponse60 = 'Other or undefined media error';
  SSmtpResponse61 = 'Media not supported';
  SSmtpResponse62 = 'Conversion required and prohibited';
  SSmtpResponse63 = 'Conversion required but not supported';
  SSmtpResponse64 = 'Conversion with loss performed';
  SSmtpResponse65 = 'Conversion failed';
  SSmtpResponse70 = 'Other or undefined security status';
  SSmtpResponse71 = 'Delivery not authorized, message refused';
  SSmtpResponse72 = 'Mailing list expansion prohibited';
  SSmtpResponse73 = 'Security conversion required but not possible';
  SSmtpResponse74 = 'Security features not supported';
  SSmtpResponse75 = 'Cryptographic failure';
  SSmtpResponse76 = 'Cryptographic algorithm not supported';
  SSmtpResponse77 = 'Message integrity failure';
  SSmtpResponseUnknown = 'Unknown response code';
  SSmtpResponseSubUnknown = 'Unknown subcode';

  SLogSmtpClass           = '[SMTP] ';
  SLogMultiLine           = 'Generating OnMultiLineResponse event';
  SLogResponse            = 'Generating OnResponse event, Code = ';
  SLogSmtpNextMessage     = 'Generating OnNextMessage event';
  SLogEncodeActionStart   = 'Generating OnEncodeAction(Start)';
  SLogEncodeActionStop    = 'Generating OnEncodeAction(Stop)';
  SLogTaskComplete        = 'Generating OnTaskComplete event ';
  SLogTaskStart           = 'Starting task: ';
  SLogNextMessageReady    = 'Next message ready';
  SLogNextMessageNotReady = 'Next message not ready';

  SLogssNoOp          = ' (ssNoOp)';
  SLogssConnect       = ' (ssConnect)';
  SLogEhlo            = ' (ssEhlo)';
  SLogHelo            = ' (ssHelo)';
  SLogMailFrom        = ' (ssMailFrom)';
  SLogRcptTo          = ' (ssRcptTo)';
  SLogRcptCc          = ' (ssRcptCc)';
  SLogRcptBcc         = ' (ssRcptBcc)';
  SLogData            = ' (ssData)';
  SLogRSet            = ' (ssRSet)';
  SLogSend            = ' (ssSend)';
  SLogSoml            = ' (ssSoml)';
  SLogSaml            = ' (ssSaml)';
  SLogVerify          = ' (ssVerify)';
  SLogExpand          = ' (ssExpand)';
  SLogHelp            = ' (ssHelp)';
  SLogTurn            = ' (ssTurn)';
  SLogQuit            = ' (ssQuit)';
  SLogSendEnvelope    = ' (ssSendEnvelope)';
  SLogSendMessage     = ' (ssSendMessage)';
  SLogSpecial         = ' (ssSpecial)';
  SLogAuthLogin       = ' (ssAuthLogin)';
  SLogAuthUser        = ' (ssAuthUser)';
  SLogAuthPass        = ' (ssAuthPass)';
  SLogstNoTask        = ' (stNoTask)';
  SLogstLogon         = ' (stLogon)';
  SLogstSendMail      = ' (stSendMail)';

  { TIpPop3Client }
  SPop3OKResp  = '+OK';
  SPop3ErrResp = '-ERR';
  SPop3NotTransacting = '%s can not be called in authentication state';
  SPop3NotAuthenticating = '%s can not be called in transaction state';
  SPop3CmdApop   = 'APOP';
  SPop3CmdTop    = 'TOP';
  SPop3CmdList   = 'LIST';
  SPop3CmdRset   = 'RSET';
  SPop3CmdRetr   = 'RETR';
  SPop3CmdDele   = 'DELE';
  SPop3CmdPass   = 'PASS';
  SPop3CmdQuit   = 'QUIT';
  SPop3CmdStat   = 'STAT';
  SPop3CmdUidl   = 'UIDL';
  SPop3CmdUser   = 'USER';
  SPop3CmdNoOp   = 'NOOP';
  SpsNoOp        = 'No operation';
  SpsConnect     = 'Connecting to server';
  SpsUser        = 'Logging on with User';
  SpsPass        = 'Logging on with Password';
  SpsStat        = 'Retrieving mailbox status';
  SpsList        = 'Retrieving mailbox list';
  SpsRetr        = 'Retrieving message';
  SpsDele        = 'Marking message for deletion';
  SpsRset        = 'Resetting messages';
  SpsApop        = 'Logging on with APOP';
  SpsTop         = 'Retrieving top of message';
  SpsUidl        = 'Retrieving mailbox UID list';
  SpsQuit        = 'Disconnecting';
  SpsSpecial     = 'Special command';
  SpsUnknown     = 'Unknown state';
  SptNone        = 'No task';
  SptLogon       = 'Logging on';
  SptList        = 'Retrieving mailbox list';
  SptUIDL        = 'Retrieving mailbox UID list';
  SptError       = 'An error occurred with the last task.';
  SptUnknown     = 'Unknown task';
  SLogPop3Class  = '[POP3] ';
  SLogState      = 'State change: ';
  sLogptNone     = ' (ptNone)';
  sLogptLogon    = ' (ptLogon)';
  sLogptList     = ' (ptList)';
  sLogptUIDL     = ' (ptUIDL)';
  SLogpsNoOp     = ' {psNoOp)';
  SLogpsConnect  = ' (psConnect)';
  SLogpsUser     = ' (psUser)';
  SLogpsPass     = ' (psPass)';
  SLogpsStat     = ' (psStat)';
  SLogosList     = ' (psList)';
  SLogpsRetr     = ' (psRetr)';
  SLogpsDele     = ' (psDele)';
  SLogpsRSet     = ' (psRSet)';
  SLogpsApop     = ' (psApop)';
  SLogpsTop      = ' (psTop)';
  SLogpsUidl     = ' (psUidl)';
  SLogpsQuit     = ' (psQuit)';
  SLogpsSpecial  = ' (psSpecial)';
  SLogPop3Message= 'Generating OnMessage event';
  SLogPop3Top    = 'Generating OnTop event';

  { TIpNntpClient }
  SNntpCmdArticle         = 'ARTICLE';
  SNntpCmdAuthPass        = 'AUTHINFO PASS';
  SNntpCmdAuthUser        = 'AUTHINFO USER';
  SNntpCmdBody            = 'BODY';
  SNntpCmdHead            = 'HEAD';
  SNntpCmdStat            = 'STAT';
  SNntpCmdDate            = 'DATE';
  SNntpCmdGroup           = 'GROUP';
  SNntpCmdHelp            = 'HELP';
  SNntpCmdLast            = 'LAST';
  SNntpCmdList            = 'LIST';
  SNntpCmdListActTimes    = 'LIST ACTIVE.TIMES';
  SNntpCmdListDistribPats = 'LIST DISTRIB.PATS';
  SNntpCmdListDistrib     = 'LIST DISTRIBUTIONS';
  SNntpCmdListNewsgroups  = 'LIST NEWSGROUPS';
  SNntpCmdListOverFmt     = 'LIST OVERVIEW.FMT';
  SNntpCmdListGroup       = 'LISTGROUP';
  SNntpCmdNewGroups       = 'NEWGROUPS';
  SNntpCmdNewNews         = 'NEWNEWS';
  SNntpCmdNext            = 'NEXT';
  SNntpCmdXOver           = 'XOVER';
  SNntpCmdPat             = 'PAT';
  SNntpCmdPost            = 'POST';
  SNntpCmdQuit            = 'QUIT';
  SNntpCmdListExt         = 'LIST EXTENSIONS';
  SnsNoOp                 = 'No operation';
  SnsConnect              = 'Connecting';
  SnsNewGroups            = 'Getting new news groups';
  SnsNewNews              = 'Getting new articles';
  SnsArticle              = 'Retrieving article';
  SnsStat                 = 'Retrieving status';
  SnsBody                 = 'Retrieving body';
  SnsHead                 = 'Retrieving heading';
  SnsGroup                = 'Selecting group';
  SnsList                 = 'Retrieving list';
  SnsLast                 = 'Selecting previous article';
  SnsNext                 = 'Selecting next article';
  SnsPrePost              = 'Preparing to post article';
  SnsPost                 = 'Posting article';
  SnsQuit                 = 'Disconnecting';
  SnsHelp                 = 'Retrieving help';
  SnsSpecial              = 'Sending special command';
  SnsAuthUser             = 'Authorizing user';
  SnsAuthPass             = 'Authorizing password';
  SnsListExt              = 'Retrieving list of extended commands';
  SnsListActiveTimes      = 'Retrieving active times';
  SnsListDistributions    = 'Retrieving list of distributions';
  SnsListDistribPats      = 'Retrieving distribution patterns';
  SnsListNewsGroups       = 'Retrieving list of available news groups';
  SnsListOverviewFmt      = 'Retrieving overview format';
  SnsListGroup            = 'Retrieving article numbers';
  SnsOver                 = 'Retrieving overview';
  SnsPat                  = 'Retrieving patterns';
  SnsDate                 = 'Retrieving server date';
  SntNoTask               = 'No task';
  SntAuthenticate         = 'Authenticating';
  SntSelectGroup          = 'Selecting Group';
  SntNewNews              = 'Retrieving new news';
  SntPostTo               = 'Posting article';

  SLogNntpClass           = '[NNTP] ';
  SLogArticle             = 'Generating OnArticle event';
  SLogntNoTask            = ' (ntNoTask)';
  SLogntAuthenticate      = ' (ntAuthenticate)';
  SLogntSelectGroup       = ' (ntSelectGroup)';
  SLogntNewNews           = ' (ntNewNews)';
  SLogntPostTo            = ' (ntPostTo)';
  SLognsNoOp              = ' (nsNoOp)';
  SLognsConnect           = ' (nsConnect)';
  SLognsNewGroups         = ' (nsNewGroups)';
  SLognsNewNews           = ' (nsNewNews)';
  SLognsArticle           = ' (nsArticle)';
  SLognsStat              = ' (nsStat)';
  SLognsBody              = ' (nsBody)';
  SLognsHead              = ' (nsHead)';
  SLognsGroup             = ' (nsGroup)';
  SLognsList              = ' (nsList)';
  SLognsLast              = ' (nsLast)';
  SLognsNext              = ' (nsNext)';
  SLognsPrePost           = ' (nsPrePost)';
  SLognsPost              = ' (nsPost)';
  SLognsQuit              = ' (nsQuit)';
  SLognsHelp              = ' (nsHelp)';
  SLognsSpecial           = ' (nsSpecial)';
  SLognsAuthUser          = ' (nsAuthUser)';
  SLognsAuthPass          = ' (nsAuthPass)';
  SLognsListExt           = ' (nsListExt)';
  SLognsListActiveTimes   = ' (nsListActiveTimes)';
  SLognsListDistributions = ' (nsListDistributions)';
  SLognsListDistribPats   = ' (nsListDistribPats)';
  SLognsListNewsGroups    = ' (nsListNewsGroups)';
  SLognsListOverviewFmt   = ' (nsListOverviewFmt)';
  SLognsListGroup         = ' (nsListGroup)';
  SLognsOver              = ' (nsOver)';
  SLognsPat               = ' (nsPat)';
  SLognsDate              = ' (nsDate)';

  { TIpHttpClient }
  HttpConnect             = 'Connected: (%s)';
  HttpDisconnect          = 'Disconnected: (%s), %s Total bytes received';
  HttpProgress            = 'Progress Made: (%s), %s bytes received';
  HttpGet                 = 'GET: (%s)';
  HttpGetError            = 'GET: (%s) FAILED';
  HttpHead                = 'HEAD: (%s)';
  HttpHeadError           = 'HEAD: (%s) FAILED';
  HttpPost                = 'POST: (%s)';
  HttpPostError           = 'POST: (%s) FAILED';
  HttpDownload            = 'Download: (%s), Error downloading';
  HttpSizeMismatch        = 'Download: (%s), Size Mismatch expecting %s , got %s';
  HttpGotHeader           = 'Download: (%s), Got Header Data';
  HttpCantLoadGraphic     = 'Unable to load graphic %s';
  HttpNoHeaderData        = 'No Header Data for Entity';

  { TIpCache }
  CacheDirNotExist        = 'Cache directory %s does not exist.';
  CacheAdding             = 'Caching item (%s = %s)';
  CacheRetrieving         = 'Loading from Cache (%s = %s)';
  CacheCheckFreshness     = 'Checking Freshness (%s)';

  { TIpCustomHtmlDataProvider }
  ProviderUnknownPicture  = 'Invalid picture format';
  ProviderUnknownFormat   = 'Don''t know how to handle %s';
  ProviderUnknownRequest  = 'Unknown request type "%s"';

  { TIpAnimationFrameList }
  sBadFrameListObject     = 'Unrecognized object of class %s in GIF Frame List';

  { TIpAnimatedImageLibImage }
  sBadImageLibFileFormat  = 'Unrecognized file format';
  sBadImageLibStream      = 'ImageLib must use TMemoryStreams';

  { TIpPNGImage }
  sPNGBadPixelDepth       = 'Unrecognized pixel depth of %d';
  sPNGMissingIHDR         = 'IHDR Chunk is missing';
  sPNGChunkIDAndLength    = 'PNG Chunk: %s  Length: %d';
  sPNGMissingIEND         = 'End of PNG found with no IEND chunk';
  sPNGEffectiveFilter     = 'Effective filter is %s';
  sPNGBadInterlaceMethod  = 'Unrecognized Interlace Method';
  sPNGDefilterPass        = 'Unfiltering Pass %d  Size: %dx%d  From: %dx%d';
  sPNGFilterChange        = 'Filter changed on Row %d to %x';
  sPNGBadColorType        = 'Unrecognized color type of %d';
  sPNGErrorConstant       = '**** ERROR ****';
  sPNGWarningConstant     = '**** WARNING ****';
  sPNGBadBitDepth         = 'Unsupported Bit Depth of %d';
  sPNGBadChunkType        = 'Unrecognized Chunk Type: %s';
  sPNGBadSignature        = 'Invalid PNG Signature';
  sPNGNoClipboard         = 'PNG Clipboard support is not supported.';
  sPNGUnsupportedFeature  = 'A %s chunk was found in the PNG File. ' +
                            'This feature is not ' +
                            'supported in this version of the PNG decoder';
  sPNGBufferTooSmall      = 'PNG Buffer too small.';
  sPNGMemoryRequired      = 'Memory required for image: %d bytes';
  sPNGGAMATooLong         = 'gAMA chunk is long';
  sPNGGAMATooShort        = 'gAMA chunk is short';
  sPNGGammaCorrection     = 'Gamma Correction: %f';
  sPNGIHDRTooLong         = 'IHDR chunk is long.';
  sPNGIHDRTooShort        = 'IHDR chunk is short.';
  sPNGImageSize           = 'Image size is %dx%d pixels';
  sPNGBitDepth            = 'Bit Depth: %d';
  sPNGColorType           = 'Color Type: %d';
  sPNGCOmpressionMethod   = 'Compression Method: %d';
  sPNGFilterMethod        = 'Filter Method: %d';
  sPNGInterlaceMethod     = 'Interlace Method: %d';
  sPNGBadPaletteLength    = 'Invalid Palette Length';
  sPNGPaletteTooLong      = 'Too many palette entries';
  sPNGPaletteEntry        = 'Palette Entry %d - Red: %d   Green: %d   Blue: %d';
  sPNGTimeTooLong         = 'tIME chunk is long.';
  sPNGTimeTooShort        = 'tIME chunk is short.';
  sPNGModificationDate    = 'Modification Date: %s';
  sPNGBadModificationTime = 'Invalid Modification Time';
  sPNGPaletteTransparency = 'Palette Transparency: %d  Alpha %d';
  sPNGTransparentColor    = 'Transparent Color: %x';
  sPNGTruncatedData       = 'Chunk data is truncated';
  sPNGTruncatedCRC        = 'CRC Code is truncated';
  sPNGCannotSave          = 'PNG Saving is not supported';

  { TIpWebImageAccess }
  sWebImageNotFound       = '%s was not found';
  sWebImageCannotLoad     = 'Cannot load %s';
  sWebImageStreamBad      = 'Cannot load image from stream';

  { TIpFtpClient }
  sFtpOpen                = 'Connected to ';
  sFtpClose               = 'Disconnected';
  sFtpLogin               = ' logged in';
  sFtpLogout              = ' logged out';
  sFtpDelete              = 'Deleting ';
  sFtpRename              = 'Renaming ';
  sFtpRetrieve            = 'Retrieving ';
  sFtpStore               = 'Storing ';
  sFtpComplete            = 'Transfer complete. ';
  sFtpBytesTransferred    = ' bytes Transferred';
  sFtpRestart             = 'Attempting to re-start transfer of ';
  sFtpTimeout             = 'Transfer timed out';
  sFtpUserAbort           = 'Transfer aborted by user';

  { Broker classes }
  sBrokerDownloadReq      = 'Download %s?';
  sBrokerDownloadTitle    = 'Download?';

  {RSA}
  sBIBufferNotAssigned = 'Buffer not assigned';

  { SSL }
  sSSLNoCertificate          = 'Certificate is not available.';
  sSSLUnsupportedEncoding    = 'Unsupported public encoding.';
  sSSLUnsupportedChiper      = 'Unsupported cipher chosen.';
  sSSLBadPublicEncoding      = 'Bad public encoding type.';
  sSSLPaddingError           = 'Padding error.';
  sSSLParserError            = 'Parsing error.';
  sSSLInvalidCipher          = 'Invalid cipher.';
  sSSLCloseNotify            = 'Server sent close notify.';
  sSSLUnexpectedMessage      = 'Server received an unexpected message.';
  sSSLBadRecordMac           = 'Server received a bad record MAC.';
  sSSLCompressionFailure     = 'Compression failure.';
  sSSLHandShakeFailure       = 'Handshake failure.';
  sSSLBadCertificate         = 'Bad certificate.';
  sSSLUnsupportedCertificate = 'Unsupported Certificate.';
  sSSLRevokedCertificate     = 'Revoked Certificate.';
  sSSLExpiredCertificate     = 'Expired Certificate.';
  sSSLUnknownCertificate     = 'Unknown Certificate.';
  sSSLIllegalParameter       = 'Illegal Parameter.';
  sSSLReadSizeMissMatch      = 'Read size miss-match.';
  sSSLReadError              = 'Read error.';
  sSSLPointerNotAssigned     = 'Pointer not assigned.';
  sSSLFailedhelloParse       = 'Did not parse server hello correctly.';
  sSSLEncryptionType         = 'Encryption type not defined.';
  sSSLBlockSizeError         = 'Block size error';
  sSSLServerNoHandShake      = 'Server did not return a handshake message.';
  sSSLServerNoServerHello    = 'Server did not return a server hello message.';
  sSSLBadCompressionValue    = 'Compression value is wrong.';
  sSSLBadCertType            = 'Cert type not found';
  sSSLBadKeyExchangeType     = 'Key exchange message expected but not received';
  sSSLBufferOverFlow         = 'Buffer overflow error.';
  sSSLNoHashType             = 'No hash type selected.';
  sSSLNoMessageEncSlected    = 'No message encoding type selected.';
  sSSLBadMac                 = 'MAC did not match.';
  sSSLSessIDToLong           = 'Session ID is longer than 32 bytes.';
  sSSLBadMD5Hash             = 'MD5 hash did not match.';
  sSSLBadSHA1Hash            = 'SHA1 hash did not match.';
  sSSLEncryptBuf2Small       = 'Encrypt buffer to small.';
  sSSLSHAbuf2Small           = 'SHA1 buffer to small.';
  sSSLBufferSizeMissMatch    = 'Buffer size miss-match.';
  sSSLNotEnoughKeyMaterail   = 'Not enough key material.';
  SSLNoPreMasterSecret       = 'No pre-master secret.';
  sSSLNoRoom                 = 'Not enough memory available to read SSL record.';
  sSSLUnprocessedData        = 'SSL data processing error.';
  sSSLConnectChange          = 'Can not change SSL status while connected.';

implementation

end.
