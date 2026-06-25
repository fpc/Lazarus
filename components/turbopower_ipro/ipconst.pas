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
  SNoStreamErr = 'Stream not assigned';
  SNoMemoryStreamErr = 'No Memory Stream assigned';

  { HTML Errors}
  SHTMLNotContainer = 'Parent is not a container';
  SHTMLLineError = 'Error "%s" at line %d, position %d';
  SHTMLCharStackOverfl = 'Character stack overflow';
  SHTMLTokenStackOverfl = 'Token stack overflow';
  SHTMLInternal = 'Internal error';
  SHTMLNoDataProvider = 'No data provider assigned';
  SHTMLResUnavail = 'Resource unavailable:';
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
  SHTMLInvScroll = 'Invalid scrolling specified';
  SHTMLDefSubmitCaption = 'Submit';
  SHTMLDefResetCaption = 'Reset';
  SHTMLDefBrowseCaption = 'Browse ...';
  SHTMLInvPicture = 'Invalid picture returned';
  SHTMLNoGraphic = 'Picture object contains no graphic object';
  SHTMLInvGraphic = 'Invalid graphic returned';
  SHTMLNoGetImage = 'No OnGetImage event handler assigned';

  { Ansi Text Stream Errors and Messages }
  SNoSeekForRead = 'No seek for read';
  SNoSeekForWrite = 'No seek for write';
  SCannotWriteToStream = 'Cannot write to stream';
  SBadSeekOrigin = 'Invalid seek origin';
  SBadLineTerminator = 'Invalid line terminator';
  SBadLineLength = 'Invalid line length';
  SBadPath = 'Path does not exist';
  SOriginFromBegin = 'When origin is soFromBeginning, Offset must be >= 0';
  SOriginFromEnd = 'When origin is soFromEnd, Offset must be <= 0';
  SMemMapFilenameRequired = 'You must specify a file name for TIpMemMapStream';
  SMemMapMustBeClosed = 'The %s method requires the TIpMemMapStream instance to be closed';
  SMemMapMustBeOpen = 'The %s method requires the TIpMemMapStream instance to be opened';

  { Mime message class errors and messages }
  SBadOffset = 'Invalid stream offset';
  SNoBoundary = 'No Mime boundary';
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

  { TIpCustomHtmlDataProvider }
  ProviderUnknownPicture  = 'Invalid picture format';

  { TIpAnimationFrameList }
  sBadFrameListObject     = 'Unrecognized object of class %s in GIF Frame List';

implementation

end.
