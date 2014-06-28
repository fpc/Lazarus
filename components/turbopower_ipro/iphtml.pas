{******************************************************************}
{*     IPHTML.PAS - HTML Browser and associated classes           *}
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
 * The Original Code is Turbo Power Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * 09/29/2007  DefaultTypeFace and FixedTypeFace are enabled
 *             FactBAParag: Incremental factor for space between lines
 *               default value is 1,
 *               proof it with values of 0.5 = {... margin-top: 0.5em; margin-bottom: 0.5em; }
 *             Delphi: adjustments
 * 10/01/2007  TextWidth of an anchor (<a name="XXXX">), before = TextWidth (' ') now is only 1
 *             Delphi: adjustments (crush when TIpHtmlPanelH was run-time created)
 * 10/03/2007  Delphi: supports jpg, png, etc
 *
 * Contributor(s):
 *
 * adem baba <adembaba@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

{off $DEFINE IP_LAZARUS_DBG}

unit IpHtml;

interface

uses
  {$IFDEF IP_LAZARUS}
  //MemCheck,
  Types,
  LCLType,
  LCLPRoc,
  GraphType,
  LCLIntf,
  LResources,
  LMessages,
  LCLMemManager,
  Translations,
  FileUtil,
  LConvEncoding,
  contnrs,
  IpHtmlTabList,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  {$IFDEF IP_LAZARUS}
    {$IFDEF UseGifImageUnit}
      GifImage,
    {$ELSE}
      IpAnim,
      {$IFDEF AndersGIFImage }
        IpAnAGif,
      {$ENDIF}
      {$IFDEF ImageLibGIFImage }
        IpAnImgL,
      {$ENDIF}
    {$ENDIF}
    {$IFDEF UsePNGGraphic}
      IpPNGImg,
    {$ENDIF}
  {$ELSE}
  GIFImage,
  JPeg, 
  {$ENDIF}
  GraphUtil,
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  Forms,
  ClipBrd,
  IpConst,
  IpStrms,
  IpUtils,
  Dialogs,
  IpMsg,
  TypInfo;

type
  {$IFNDEF IP_LAZARUS}
    PtrInt = Longint;
  {$ENDIF}
  {Note: Some of the code below relies on the fact that
   the end tag (when present) immediately follows the
   start tag.}

//
// To preprocess this file use on command line
// lua lpp.lua onefile-with-lua-inside
//

(*lua
--
-- the following tables should be ordered
--
htmlTags = {
  {'!--', false, 'COMMENT'},
  {'!DOCTYPE', false, 'DOCTYPE'},
  {'<eof>', false, 'Eof'},
  {'<text>', false, 'Text'},
  {'<unknown>', false, 'Unknown'},
  {'A', true},
  {'ABBR', true},
  {'ACRONYM', true},
  {'ADDRESS', true},
  {'APPLET', true},
  {'AREA', false},
  {'B', true},
  {'BASE', false},
  {'BASEFONT', false},
  {'BIG', true},
  {'BLINK', true},
  {'BLOCKQUOTE', true},
  {'BODY', true},
  {'BR', false},
  {'BUTTON', true},
  {'CAPTION', true},
  {'CENTER', true},
  {'CITE', true},
  {'CODE', true},
  {'COL', false},
  {'COLGROUP', true},
  {'DD', true},
  {'DEL', true},
  {'DFN', true},
  {'DIR', true},
  {'DIV', true},
  {'DL', true},
  {'DT', true},
  {'EM', true},
  {'FIELDSET', true},
  {'FONT', true},
  {'FORM', true},
  {'FRAME', false},
  {'FRAMESET', true},
  {'H1', true},
  {'H2', true},
  {'H3', true},
  {'H4', true},
  {'H5', true},
  {'H6', true},
  {'HEAD', true},
  {'HR', false},
  {'HTML', true},
  {'I', true},
  {'IFRAME', true},
  {'IMG', false},
  {'INPUT', false},
  {'INS', true},
  {'ISINDEX', false},
  {'KBD', true},
  {'LABEL', true},
  {'LEFT', true},
  {'LEGEND', true},
  {'LI', true},
  {'LINK', false},
  {'MAP', true},
  {'MENU', true},
  {'META', false},
  {'NOBR', true},
  {'NOFRAMES', true},
  {'NOSCRIPT', true},
  {'OBJECT', true},
  {'OL', true},
  {'OPTGROUP', true},
  {'OPTION', true},
  {'P', true},
  {'PARAM', false},
  {'PRE', true},
  {'Q', true},
  {'RIGHT', true},
  {'S', true},
  {'SAMP', true},
  {'SCRIPT', true},
  {'SELECT', true},
  {'SMALL', true},
  {'SPAN', true},
  {'STRIKE', true},
  {'STRONG', true},
  {'STYLE', true},
  {'SUB', true},
  {'SUP', true},
  {'TABLE', true},
  {'TBODY', true},
  {'TD', true},
  {'TEXTAREA', true},
  {'TFOOT', true},
  {'TH', true},
  {'THEAD', true},
  {'TITLE', true},
  {'TR', true},
  {'TT', true},
  {'U', true},
  {'UL', true},
  {'VAR', true},
}

table.sort(htmlTags, function(a,b) return a[1] < b[1] end)

function getHtmlTagsTable()
  local k,v, tbl
  tbl = {}
  for k,v in(htmlTags) do
      table.insert(tbl, v[1])
      if v[2] then table.insert(tbl, '/' .. v[2]) end
  end
  table.sort(tbl)
  return tbl
end

function genTIpHtmlToken()
  local k,v, nEnd
  nEnd = #htmlTags
  _putsnl_('  TIpHtmlToken = (')
  for k,v in pairs(htmlTags) do
      _puts_('\tIpHtmlTag')
      if v[3] then
         _puts_(v[3])
      else
         _puts_(v[1])
      end
      if v[2] then
         _puts_(', IpHtmlTag' .. v[1] .. 'end' )
      end
      if k < nEnd then  _putsnl_(', ') end
  end
  _putsnl_('  );')
end

function genIpEndTokenSet()
  local k,v, nEnd
  nEnd = #htmlTags
  _putsnl_('  IpEndTokenSet : TIpHtmlTokenSet = [')
  for k,v in pairs(htmlTags) do
      if v[2] then
         _puts_('\tIpHtmlTag' .. v[1] .. 'end' )
         if k < nEnd then  _putsnl_(', ') end
      end
  end
  _putsnl_('  ];')
end

function genIpHtmlTokens()
  local k,v, tbl, nEnd, line
  tbl = {}
  for k,v in pairs(htmlTags) do
      line = "\t(pc:'" .. v[1] .. "'; tk:IpHtmlTag"
      if v[3] then
         line = line .. v[3]
      else
         line = line .. v[1]
      end
	  line = line .. ')'
	  table.insert(tbl, line)

      if v[2] then
		  line = "\t(pc:'/" .. v[1] .. "'; tk:IpHtmlTag"
		  if v[3] then
			 line = line .. v[3]
		  else
			 line = line .. v[1]
		  end
		  line = line .. 'end)'
		  table.insert(tbl, line)
      end

  end
  table.sort(tbl)
  nEnd = #tbl
  _putsnl_('  IpHtmlTokens : array[0..' .. nEnd-1 ..
	'] of record\n\tpc: PAnsiChar;\n\ttk: TIpHtmlToken;\n  end = ( // alphabetically ordered')
  _puts_(table.concat(tbl,',\n'))
  _putsnl_('  );')
end

htmlNodeAttributes = {
'ACCEPT',
'ACCEPT-CHARSET',
'ACTION',
'ALIGN',
'ALINK',
'ALT',
'ARCHIVE',
'BACKGROUND',
'BGCOLOR',
'BORDER',
'CELLPADDING',
'CELLSPACING',
'CHECKED',
'CITE',
'CLASS',
'CLASSID',
'CLEAR',
'CODE',
'CODEBASE',
'CODETYPE',
'COLOR',
'COLS',
'COLSPAN',
'COMBOBOX',
'COMPACT',
'CONTENT',
'COORDS',
'DATA',
'DATETIME',
'DECLARE',
'DIR',
'DISABLED',
'ENCTYPE',
'FACE',
'FRAME',
'HEIGHT',
'HREF',
'HSPACE',
'HTTP-EQUIV',
'ID',
'ISMAP',
'LABEL',
'LANG',
'LANGUAGE',
'LINK',
'LONGDESC',
'MARGINHEIGHT',
'MARGINWIDTH',
'MAXLENGTH',
'MEDIA',
'METHOD',
'MULTIPLE',
'NAME',
'NOHREF',
'NORESIZE',
'NOSHADE',
'NOWRAP',
'OBJECT',
'PROMPT',
'READONLY',
'REL',
'REV',
'ROWS',
'ROWSPAN',
'RULES',
'SCHEME',
'SCROLLING',
'SELECTED',
'SHAPE',
'SIZE',
'SPAN',
'SRC',
'STANDBY',
'START',
'STYLE',
'SUMMARY',
'TABINDEX',
'TARGET',
'TEXT',
'TITLE',
'TYPE',
'USEMAP',
'VALIGN',
'VALUE',
'VALUETYPE',
'VERSION',
'VLINK',
'VSPACE',
'WIDTH',
}

table.sort(htmlNodeAttributes)

function genHtmlNodeAttributesSet()
  local k,v, nEnd
  nEnd = #htmlNodeAttributes
  _putsnl_('  TIpHtmlAttributesSet = (')
  for k,v in pairs(htmlNodeAttributes) do
      if (k % 5) == 0 then _putsnl_('') end
      _puts_('htmlAttr' .. v:gsub('-', '_') )
      if k < nEnd then  _puts_(', ') end
  end
  _putsnl_('  );')
end

function genHtmlNodeAttributesNames()
  local k,v, nEnd
  nEnd = #htmlNodeAttributes
  _putsnl_("  TIpHtmlAttributesNames : array[TIpHtmlAttributesSet] of PAnsiChar = (")
  for k,v in pairs(htmlNodeAttributes) do
      if (k % 5) == 0 then _putsnl_('') end
      _puts_("'" .. v .. "'")
      if k < nEnd then  _puts_(', ') end
  end
  _putsnl_('  );')
end

lua*)

//#genTIpHtmlToken()
// generated-code:begin
  TIpHtmlToken = (
	IpHtmlTagCOMMENT, 
	IpHtmlTagDOCTYPE, 
	IpHtmlTagEof, 
	IpHtmlTagText, 
	IpHtmlTagUnknown, 
	IpHtmlTagA, IpHtmlTagAend, 
	IpHtmlTagABBR, IpHtmlTagABBRend, 
	IpHtmlTagACRONYM, IpHtmlTagACRONYMend, 
	IpHtmlTagADDRESS, IpHtmlTagADDRESSend, 
	IpHtmlTagAPPLET, IpHtmlTagAPPLETend, 
	IpHtmlTagAREA, 
	IpHtmlTagB, IpHtmlTagBend, 
	IpHtmlTagBASE, 
	IpHtmlTagBASEFONT, 
	IpHtmlTagBIG, IpHtmlTagBIGend, 
	IpHtmlTagBLINK, IpHtmlTagBLINKend, 
	IpHtmlTagBLOCKQUOTE, IpHtmlTagBLOCKQUOTEend, 
	IpHtmlTagBODY, IpHtmlTagBODYend, 
	IpHtmlTagBR, 
	IpHtmlTagBUTTON, IpHtmlTagBUTTONend, 
	IpHtmlTagCAPTION, IpHtmlTagCAPTIONend, 
	IpHtmlTagCENTER, IpHtmlTagCENTERend, 
	IpHtmlTagCITE, IpHtmlTagCITEend, 
	IpHtmlTagCODE, IpHtmlTagCODEend, 
	IpHtmlTagCOL, 
	IpHtmlTagCOLGROUP, IpHtmlTagCOLGROUPend, 
	IpHtmlTagDD, IpHtmlTagDDend, 
	IpHtmlTagDEL, IpHtmlTagDELend, 
	IpHtmlTagDFN, IpHtmlTagDFNend, 
	IpHtmlTagDIR, IpHtmlTagDIRend, 
	IpHtmlTagDIV, IpHtmlTagDIVend, 
	IpHtmlTagDL, IpHtmlTagDLend, 
	IpHtmlTagDT, IpHtmlTagDTend, 
	IpHtmlTagEM, IpHtmlTagEMend, 
	IpHtmlTagFIELDSET, IpHtmlTagFIELDSETend, 
	IpHtmlTagFONT, IpHtmlTagFONTend, 
	IpHtmlTagFORM, IpHtmlTagFORMend, 
	IpHtmlTagFRAME, 
	IpHtmlTagFRAMESET, IpHtmlTagFRAMESETend, 
	IpHtmlTagH1, IpHtmlTagH1end, 
	IpHtmlTagH2, IpHtmlTagH2end, 
	IpHtmlTagH3, IpHtmlTagH3end, 
	IpHtmlTagH4, IpHtmlTagH4end, 
	IpHtmlTagH5, IpHtmlTagH5end, 
	IpHtmlTagH6, IpHtmlTagH6end, 
	IpHtmlTagHEAD, IpHtmlTagHEADend, 
	IpHtmlTagHR, 
	IpHtmlTagHTML, IpHtmlTagHTMLend, 
	IpHtmlTagI, IpHtmlTagIend, 
	IpHtmlTagIFRAME, IpHtmlTagIFRAMEend, 
	IpHtmlTagIMG, 
	IpHtmlTagINPUT, 
	IpHtmlTagINS, IpHtmlTagINSend, 
	IpHtmlTagISINDEX, 
	IpHtmlTagKBD, IpHtmlTagKBDend, 
	IpHtmlTagLABEL, IpHtmlTagLABELend, 
	IpHtmlTagLEFT, IpHtmlTagLEFTend, 
	IpHtmlTagLEGEND, IpHtmlTagLEGENDend, 
	IpHtmlTagLI, IpHtmlTagLIend, 
	IpHtmlTagLINK, 
	IpHtmlTagMAP, IpHtmlTagMAPend, 
	IpHtmlTagMENU, IpHtmlTagMENUend, 
	IpHtmlTagMETA, 
	IpHtmlTagNOBR, IpHtmlTagNOBRend, 
	IpHtmlTagNOFRAMES, IpHtmlTagNOFRAMESend, 
	IpHtmlTagNOSCRIPT, IpHtmlTagNOSCRIPTend, 
	IpHtmlTagOBJECT, IpHtmlTagOBJECTend, 
	IpHtmlTagOL, IpHtmlTagOLend, 
	IpHtmlTagOPTGROUP, IpHtmlTagOPTGROUPend, 
	IpHtmlTagOPTION, IpHtmlTagOPTIONend, 
	IpHtmlTagP, IpHtmlTagPend, 
	IpHtmlTagPARAM, 
	IpHtmlTagPRE, IpHtmlTagPREend, 
	IpHtmlTagQ, IpHtmlTagQend, 
	IpHtmlTagRIGHT, IpHtmlTagRIGHTend, 
	IpHtmlTagS, IpHtmlTagSend, 
	IpHtmlTagSAMP, IpHtmlTagSAMPend, 
	IpHtmlTagSCRIPT, IpHtmlTagSCRIPTend, 
	IpHtmlTagSELECT, IpHtmlTagSELECTend, 
	IpHtmlTagSMALL, IpHtmlTagSMALLend, 
	IpHtmlTagSPAN, IpHtmlTagSPANend, 
	IpHtmlTagSTRIKE, IpHtmlTagSTRIKEend, 
	IpHtmlTagSTRONG, IpHtmlTagSTRONGend, 
	IpHtmlTagSTYLE, IpHtmlTagSTYLEend, 
	IpHtmlTagSUB, IpHtmlTagSUBend, 
	IpHtmlTagSUP, IpHtmlTagSUPend, 
	IpHtmlTagTABLE, IpHtmlTagTABLEend, 
	IpHtmlTagTBODY, IpHtmlTagTBODYend, 
	IpHtmlTagTD, IpHtmlTagTDend, 
	IpHtmlTagTEXTAREA, IpHtmlTagTEXTAREAend, 
	IpHtmlTagTFOOT, IpHtmlTagTFOOTend, 
	IpHtmlTagTH, IpHtmlTagTHend, 
	IpHtmlTagTHEAD, IpHtmlTagTHEADend, 
	IpHtmlTagTITLE, IpHtmlTagTITLEend, 
	IpHtmlTagTR, IpHtmlTagTRend, 
	IpHtmlTagTT, IpHtmlTagTTend, 
	IpHtmlTagU, IpHtmlTagUend, 
	IpHtmlTagUL, IpHtmlTagULend, 
	IpHtmlTagVAR, IpHtmlTagVARend  );
// generated-code:end
  TIpHtmlTokenSet = set of TIpHtmlToken;

//#genHtmlNodeAttributesSet()
// generated-code:begin
  TIpHtmlAttributesSet = (
htmlAttrACCEPT, htmlAttrACCEPT_CHARSET, htmlAttrACTION, htmlAttrALIGN, 
htmlAttrALINK, htmlAttrALT, htmlAttrARCHIVE, htmlAttrBACKGROUND, htmlAttrBGCOLOR, 
htmlAttrBORDER, htmlAttrCELLPADDING, htmlAttrCELLSPACING, htmlAttrCHECKED, htmlAttrCITE, 
htmlAttrCLASS, htmlAttrCLASSID, htmlAttrCLEAR, htmlAttrCODE, htmlAttrCODEBASE, 
htmlAttrCODETYPE, htmlAttrCOLOR, htmlAttrCOLS, htmlAttrCOLSPAN, htmlAttrCOMBOBOX, 
htmlAttrCOMPACT, htmlAttrCONTENT, htmlAttrCOORDS, htmlAttrDATA, htmlAttrDATETIME, 
htmlAttrDECLARE, htmlAttrDIR, htmlAttrDISABLED, htmlAttrENCTYPE, htmlAttrFACE, 
htmlAttrFRAME, htmlAttrHEIGHT, htmlAttrHREF, htmlAttrHSPACE, htmlAttrHTTP_EQUIV, 
htmlAttrID, htmlAttrISMAP, htmlAttrLABEL, htmlAttrLANG, htmlAttrLANGUAGE, 
htmlAttrLINK, htmlAttrLONGDESC, htmlAttrMARGINHEIGHT, htmlAttrMARGINWIDTH, htmlAttrMAXLENGTH, 
htmlAttrMEDIA, htmlAttrMETHOD, htmlAttrMULTIPLE, htmlAttrNAME, htmlAttrNOHREF, 
htmlAttrNORESIZE, htmlAttrNOSHADE, htmlAttrNOWRAP, htmlAttrOBJECT, htmlAttrPROMPT, 
htmlAttrREADONLY, htmlAttrREL, htmlAttrREV, htmlAttrROWS, htmlAttrROWSPAN, 
htmlAttrRULES, htmlAttrSCHEME, htmlAttrSCROLLING, htmlAttrSELECTED, htmlAttrSHAPE, 
htmlAttrSIZE, htmlAttrSPAN, htmlAttrSRC, htmlAttrSTANDBY, htmlAttrSTART, 
htmlAttrSTYLE, htmlAttrSUMMARY, htmlAttrTABINDEX, htmlAttrTARGET, htmlAttrTEXT, 
htmlAttrTITLE, htmlAttrTYPE, htmlAttrUSEMAP, htmlAttrVALIGN, htmlAttrVALUE, 
htmlAttrVALUETYPE, htmlAttrVERSION, htmlAttrVLINK, htmlAttrVSPACE, htmlAttrWIDTH  );
// generated-code:end

const
//#genIpEndTokenSet()
// generated-code:begin
  IpEndTokenSet : TIpHtmlTokenSet = [
	IpHtmlTagAend, 
	IpHtmlTagABBRend, 
	IpHtmlTagACRONYMend, 
	IpHtmlTagADDRESSend, 
	IpHtmlTagAPPLETend, 
	IpHtmlTagBend, 
	IpHtmlTagBIGend, 
	IpHtmlTagBLINKend, 
	IpHtmlTagBLOCKQUOTEend, 
	IpHtmlTagBODYend, 
	IpHtmlTagBUTTONend, 
	IpHtmlTagCAPTIONend, 
	IpHtmlTagCENTERend, 
	IpHtmlTagCITEend, 
	IpHtmlTagCODEend, 
	IpHtmlTagCOLGROUPend, 
	IpHtmlTagDDend, 
	IpHtmlTagDELend, 
	IpHtmlTagDFNend, 
	IpHtmlTagDIRend, 
	IpHtmlTagDIVend, 
	IpHtmlTagDLend, 
	IpHtmlTagDTend, 
	IpHtmlTagEMend, 
	IpHtmlTagFIELDSETend, 
	IpHtmlTagFONTend, 
	IpHtmlTagFORMend, 
	IpHtmlTagFRAMESETend, 
	IpHtmlTagH1end, 
	IpHtmlTagH2end, 
	IpHtmlTagH3end, 
	IpHtmlTagH4end, 
	IpHtmlTagH5end, 
	IpHtmlTagH6end, 
	IpHtmlTagHEADend, 
	IpHtmlTagHTMLend, 
	IpHtmlTagIend, 
	IpHtmlTagIFRAMEend, 
	IpHtmlTagINSend, 
	IpHtmlTagKBDend, 
	IpHtmlTagLABELend, 
	IpHtmlTagLEFTend, 
	IpHtmlTagLEGENDend, 
	IpHtmlTagLIend, 
	IpHtmlTagMAPend, 
	IpHtmlTagMENUend, 
	IpHtmlTagNOBRend, 
	IpHtmlTagNOFRAMESend, 
	IpHtmlTagNOSCRIPTend, 
	IpHtmlTagOBJECTend, 
	IpHtmlTagOLend, 
	IpHtmlTagOPTGROUPend, 
	IpHtmlTagOPTIONend, 
	IpHtmlTagPend, 
	IpHtmlTagPREend, 
	IpHtmlTagQend, 
	IpHtmlTagRIGHTend, 
	IpHtmlTagSend, 
	IpHtmlTagSAMPend, 
	IpHtmlTagSCRIPTend, 
	IpHtmlTagSELECTend, 
	IpHtmlTagSMALLend, 
	IpHtmlTagSPANend, 
	IpHtmlTagSTRIKEend, 
	IpHtmlTagSTRONGend, 
	IpHtmlTagSTYLEend, 
	IpHtmlTagSUBend, 
	IpHtmlTagSUPend, 
	IpHtmlTagTABLEend, 
	IpHtmlTagTBODYend, 
	IpHtmlTagTDend, 
	IpHtmlTagTEXTAREAend, 
	IpHtmlTagTFOOTend, 
	IpHtmlTagTHend, 
	IpHtmlTagTHEADend, 
	IpHtmlTagTITLEend, 
	IpHtmlTagTRend, 
	IpHtmlTagTTend, 
	IpHtmlTagUend, 
	IpHtmlTagULend, 
	IpHtmlTagVARend  ];
// generated-code:end

//#genIpHtmlTokens()
// generated-code:begin
  IpHtmlTokens : array[0..179] of record
	pc: PAnsiChar;
	tk: TIpHtmlToken;
  end = ( // alphabetically ordered
	(pc:'!--'; tk:IpHtmlTagCOMMENT),
	(pc:'!DOCTYPE'; tk:IpHtmlTagDOCTYPE),
	(pc:'/A'; tk:IpHtmlTagAend),
	(pc:'/ABBR'; tk:IpHtmlTagABBRend),
	(pc:'/ACRONYM'; tk:IpHtmlTagACRONYMend),
	(pc:'/ADDRESS'; tk:IpHtmlTagADDRESSend),
	(pc:'/APPLET'; tk:IpHtmlTagAPPLETend),
	(pc:'/B'; tk:IpHtmlTagBend),
	(pc:'/BIG'; tk:IpHtmlTagBIGend),
	(pc:'/BLINK'; tk:IpHtmlTagBLINKend),
	(pc:'/BLOCKQUOTE'; tk:IpHtmlTagBLOCKQUOTEend),
	(pc:'/BODY'; tk:IpHtmlTagBODYend),
	(pc:'/BUTTON'; tk:IpHtmlTagBUTTONend),
	(pc:'/CAPTION'; tk:IpHtmlTagCAPTIONend),
	(pc:'/CENTER'; tk:IpHtmlTagCENTERend),
	(pc:'/CITE'; tk:IpHtmlTagCITEend),
	(pc:'/CODE'; tk:IpHtmlTagCODEend),
	(pc:'/COLGROUP'; tk:IpHtmlTagCOLGROUPend),
	(pc:'/DD'; tk:IpHtmlTagDDend),
	(pc:'/DEL'; tk:IpHtmlTagDELend),
	(pc:'/DFN'; tk:IpHtmlTagDFNend),
	(pc:'/DIR'; tk:IpHtmlTagDIRend),
	(pc:'/DIV'; tk:IpHtmlTagDIVend),
	(pc:'/DL'; tk:IpHtmlTagDLend),
	(pc:'/DT'; tk:IpHtmlTagDTend),
	(pc:'/EM'; tk:IpHtmlTagEMend),
	(pc:'/FIELDSET'; tk:IpHtmlTagFIELDSETend),
	(pc:'/FONT'; tk:IpHtmlTagFONTend),
	(pc:'/FORM'; tk:IpHtmlTagFORMend),
	(pc:'/FRAMESET'; tk:IpHtmlTagFRAMESETend),
	(pc:'/H1'; tk:IpHtmlTagH1end),
	(pc:'/H2'; tk:IpHtmlTagH2end),
	(pc:'/H3'; tk:IpHtmlTagH3end),
	(pc:'/H4'; tk:IpHtmlTagH4end),
	(pc:'/H5'; tk:IpHtmlTagH5end),
	(pc:'/H6'; tk:IpHtmlTagH6end),
	(pc:'/HEAD'; tk:IpHtmlTagHEADend),
	(pc:'/HTML'; tk:IpHtmlTagHTMLend),
	(pc:'/I'; tk:IpHtmlTagIend),
	(pc:'/IFRAME'; tk:IpHtmlTagIFRAMEend),
	(pc:'/INS'; tk:IpHtmlTagINSend),
	(pc:'/KBD'; tk:IpHtmlTagKBDend),
	(pc:'/LABEL'; tk:IpHtmlTagLABELend),
	(pc:'/LEFT'; tk:IpHtmlTagLEFTend),
	(pc:'/LEGEND'; tk:IpHtmlTagLEGENDend),
	(pc:'/LI'; tk:IpHtmlTagLIend),
	(pc:'/MAP'; tk:IpHtmlTagMAPend),
	(pc:'/MENU'; tk:IpHtmlTagMENUend),
	(pc:'/NOBR'; tk:IpHtmlTagNOBRend),
	(pc:'/NOFRAMES'; tk:IpHtmlTagNOFRAMESend),
	(pc:'/NOSCRIPT'; tk:IpHtmlTagNOSCRIPTend),
	(pc:'/OBJECT'; tk:IpHtmlTagOBJECTend),
	(pc:'/OL'; tk:IpHtmlTagOLend),
	(pc:'/OPTGROUP'; tk:IpHtmlTagOPTGROUPend),
	(pc:'/OPTION'; tk:IpHtmlTagOPTIONend),
	(pc:'/P'; tk:IpHtmlTagPend),
	(pc:'/PRE'; tk:IpHtmlTagPREend),
	(pc:'/Q'; tk:IpHtmlTagQend),
	(pc:'/RIGHT'; tk:IpHtmlTagRIGHTend),
	(pc:'/S'; tk:IpHtmlTagSend),
	(pc:'/SAMP'; tk:IpHtmlTagSAMPend),
	(pc:'/SCRIPT'; tk:IpHtmlTagSCRIPTend),
	(pc:'/SELECT'; tk:IpHtmlTagSELECTend),
	(pc:'/SMALL'; tk:IpHtmlTagSMALLend),
	(pc:'/SPAN'; tk:IpHtmlTagSPANend),
	(pc:'/STRIKE'; tk:IpHtmlTagSTRIKEend),
	(pc:'/STRONG'; tk:IpHtmlTagSTRONGend),
	(pc:'/STYLE'; tk:IpHtmlTagSTYLEend),
	(pc:'/SUB'; tk:IpHtmlTagSUBend),
	(pc:'/SUP'; tk:IpHtmlTagSUPend),
	(pc:'/TABLE'; tk:IpHtmlTagTABLEend),
	(pc:'/TBODY'; tk:IpHtmlTagTBODYend),
	(pc:'/TD'; tk:IpHtmlTagTDend),
	(pc:'/TEXTAREA'; tk:IpHtmlTagTEXTAREAend),
	(pc:'/TFOOT'; tk:IpHtmlTagTFOOTend),
	(pc:'/TH'; tk:IpHtmlTagTHend),
	(pc:'/THEAD'; tk:IpHtmlTagTHEADend),
	(pc:'/TITLE'; tk:IpHtmlTagTITLEend),
	(pc:'/TR'; tk:IpHtmlTagTRend),
	(pc:'/TT'; tk:IpHtmlTagTTend),
	(pc:'/U'; tk:IpHtmlTagUend),
	(pc:'/UL'; tk:IpHtmlTagULend),
	(pc:'/VAR'; tk:IpHtmlTagVARend),
	(pc:'<eof>'; tk:IpHtmlTagEof),
	(pc:'<text>'; tk:IpHtmlTagText),
	(pc:'<unknown>'; tk:IpHtmlTagUnknown),
	(pc:'A'; tk:IpHtmlTagA),
	(pc:'ABBR'; tk:IpHtmlTagABBR),
	(pc:'ACRONYM'; tk:IpHtmlTagACRONYM),
	(pc:'ADDRESS'; tk:IpHtmlTagADDRESS),
	(pc:'APPLET'; tk:IpHtmlTagAPPLET),
	(pc:'AREA'; tk:IpHtmlTagAREA),
	(pc:'B'; tk:IpHtmlTagB),
	(pc:'BASE'; tk:IpHtmlTagBASE),
	(pc:'BASEFONT'; tk:IpHtmlTagBASEFONT),
	(pc:'BIG'; tk:IpHtmlTagBIG),
	(pc:'BLINK'; tk:IpHtmlTagBLINK),
	(pc:'BLOCKQUOTE'; tk:IpHtmlTagBLOCKQUOTE),
	(pc:'BODY'; tk:IpHtmlTagBODY),
	(pc:'BR'; tk:IpHtmlTagBR),
	(pc:'BUTTON'; tk:IpHtmlTagBUTTON),
	(pc:'CAPTION'; tk:IpHtmlTagCAPTION),
	(pc:'CENTER'; tk:IpHtmlTagCENTER),
	(pc:'CITE'; tk:IpHtmlTagCITE),
	(pc:'CODE'; tk:IpHtmlTagCODE),
	(pc:'COL'; tk:IpHtmlTagCOL),
	(pc:'COLGROUP'; tk:IpHtmlTagCOLGROUP),
	(pc:'DD'; tk:IpHtmlTagDD),
	(pc:'DEL'; tk:IpHtmlTagDEL),
	(pc:'DFN'; tk:IpHtmlTagDFN),
	(pc:'DIR'; tk:IpHtmlTagDIR),
	(pc:'DIV'; tk:IpHtmlTagDIV),
	(pc:'DL'; tk:IpHtmlTagDL),
	(pc:'DT'; tk:IpHtmlTagDT),
	(pc:'EM'; tk:IpHtmlTagEM),
	(pc:'FIELDSET'; tk:IpHtmlTagFIELDSET),
	(pc:'FONT'; tk:IpHtmlTagFONT),
	(pc:'FORM'; tk:IpHtmlTagFORM),
	(pc:'FRAME'; tk:IpHtmlTagFRAME),
	(pc:'FRAMESET'; tk:IpHtmlTagFRAMESET),
	(pc:'H1'; tk:IpHtmlTagH1),
	(pc:'H2'; tk:IpHtmlTagH2),
	(pc:'H3'; tk:IpHtmlTagH3),
	(pc:'H4'; tk:IpHtmlTagH4),
	(pc:'H5'; tk:IpHtmlTagH5),
	(pc:'H6'; tk:IpHtmlTagH6),
	(pc:'HEAD'; tk:IpHtmlTagHEAD),
	(pc:'HR'; tk:IpHtmlTagHR),
	(pc:'HTML'; tk:IpHtmlTagHTML),
	(pc:'I'; tk:IpHtmlTagI),
	(pc:'IFRAME'; tk:IpHtmlTagIFRAME),
	(pc:'IMG'; tk:IpHtmlTagIMG),
	(pc:'INPUT'; tk:IpHtmlTagINPUT),
	(pc:'INS'; tk:IpHtmlTagINS),
	(pc:'ISINDEX'; tk:IpHtmlTagISINDEX),
	(pc:'KBD'; tk:IpHtmlTagKBD),
	(pc:'LABEL'; tk:IpHtmlTagLABEL),
	(pc:'LEFT'; tk:IpHtmlTagLEFT),
	(pc:'LEGEND'; tk:IpHtmlTagLEGEND),
	(pc:'LI'; tk:IpHtmlTagLI),
	(pc:'LINK'; tk:IpHtmlTagLINK),
	(pc:'MAP'; tk:IpHtmlTagMAP),
	(pc:'MENU'; tk:IpHtmlTagMENU),
	(pc:'META'; tk:IpHtmlTagMETA),
	(pc:'NOBR'; tk:IpHtmlTagNOBR),
	(pc:'NOFRAMES'; tk:IpHtmlTagNOFRAMES),
	(pc:'NOSCRIPT'; tk:IpHtmlTagNOSCRIPT),
	(pc:'OBJECT'; tk:IpHtmlTagOBJECT),
	(pc:'OL'; tk:IpHtmlTagOL),
	(pc:'OPTGROUP'; tk:IpHtmlTagOPTGROUP),
	(pc:'OPTION'; tk:IpHtmlTagOPTION),
	(pc:'P'; tk:IpHtmlTagP),
	(pc:'PARAM'; tk:IpHtmlTagPARAM),
	(pc:'PRE'; tk:IpHtmlTagPRE),
	(pc:'Q'; tk:IpHtmlTagQ),
	(pc:'RIGHT'; tk:IpHtmlTagRIGHT),
	(pc:'S'; tk:IpHtmlTagS),
	(pc:'SAMP'; tk:IpHtmlTagSAMP),
	(pc:'SCRIPT'; tk:IpHtmlTagSCRIPT),
	(pc:'SELECT'; tk:IpHtmlTagSELECT),
	(pc:'SMALL'; tk:IpHtmlTagSMALL),
	(pc:'SPAN'; tk:IpHtmlTagSPAN),
	(pc:'STRIKE'; tk:IpHtmlTagSTRIKE),
	(pc:'STRONG'; tk:IpHtmlTagSTRONG),
	(pc:'STYLE'; tk:IpHtmlTagSTYLE),
	(pc:'SUB'; tk:IpHtmlTagSUB),
	(pc:'SUP'; tk:IpHtmlTagSUP),
	(pc:'TABLE'; tk:IpHtmlTagTABLE),
	(pc:'TBODY'; tk:IpHtmlTagTBODY),
	(pc:'TD'; tk:IpHtmlTagTD),
	(pc:'TEXTAREA'; tk:IpHtmlTagTEXTAREA),
	(pc:'TFOOT'; tk:IpHtmlTagTFOOT),
	(pc:'TH'; tk:IpHtmlTagTH),
	(pc:'THEAD'; tk:IpHtmlTagTHEAD),
	(pc:'TITLE'; tk:IpHtmlTagTITLE),
	(pc:'TR'; tk:IpHtmlTagTR),
	(pc:'TT'; tk:IpHtmlTagTT),
	(pc:'U'; tk:IpHtmlTagU),
	(pc:'UL'; tk:IpHtmlTagUL),
	(pc:'VAR'; tk:IpHtmlTagVAR)  );
// generated-code:end

//#genHtmlNodeAttributesNames()
// generated-code:begin
  TIpHtmlAttributesNames : array[TIpHtmlAttributesSet] of PAnsiChar = (
'ACCEPT', 'ACCEPT-CHARSET', 'ACTION', 'ALIGN', 
'ALINK', 'ALT', 'ARCHIVE', 'BACKGROUND', 'BGCOLOR', 
'BORDER', 'CELLPADDING', 'CELLSPACING', 'CHECKED', 'CITE', 
'CLASS', 'CLASSID', 'CLEAR', 'CODE', 'CODEBASE', 
'CODETYPE', 'COLOR', 'COLS', 'COLSPAN', 'COMBOBOX', 
'COMPACT', 'CONTENT', 'COORDS', 'DATA', 'DATETIME', 
'DECLARE', 'DIR', 'DISABLED', 'ENCTYPE', 'FACE', 
'FRAME', 'HEIGHT', 'HREF', 'HSPACE', 'HTTP-EQUIV', 
'ID', 'ISMAP', 'LABEL', 'LANG', 'LANGUAGE', 
'LINK', 'LONGDESC', 'MARGINHEIGHT', 'MARGINWIDTH', 'MAXLENGTH', 
'MEDIA', 'METHOD', 'MULTIPLE', 'NAME', 'NOHREF', 
'NORESIZE', 'NOSHADE', 'NOWRAP', 'OBJECT', 'PROMPT', 
'READONLY', 'REL', 'REV', 'ROWS', 'ROWSPAN', 
'RULES', 'SCHEME', 'SCROLLING', 'SELECTED', 'SHAPE', 
'SIZE', 'SPAN', 'SRC', 'STANDBY', 'START', 
'STYLE', 'SUMMARY', 'TABINDEX', 'TARGET', 'TEXT', 
'TITLE', 'TYPE', 'USEMAP', 'VALIGN', 'VALUE', 
'VALUETYPE', 'VERSION', 'VLINK', 'VSPACE', 'WIDTH'  );
// generated-code:end

const
  IPMAXFRAMES = 256; {maximum number of frames in a single frameset}
  MAXINTS = 4096; {buffer size - this should be way more than needed}
  TINTARRGROWFACTOR = 64;
  DEFAULT_PRINTMARGIN = 0.5; {inches}
  FONTSIZESVALUSARRAY : array[0..6] of integer = (8,10,12,14,18,24,36);
  MAXWORDS = 65536;

type
  {$IFDEF IP_LAZARUS}
  TIpEnumItemsMethod = TLCLEnumItemsMethod;
  TIpHtmlPoolManager = class(TLCLNonFreeMemManager)
  public
    constructor Create(TheItemSize, MaxItems : DWord);
    function NewItm : Pointer;
  end;
  {$ELSE}
  TIpEnumItemsMethod = procedure(Item: Pointer) of object;
  TIpHtmlPoolManager = class
  private
    Root : Pointer;
    {Top : Pointer;}
    NextPage : Pointer;
    Next : Pointer;
    InternalSize : DWord;
    Critical : TRtlCriticalSection;
    procedure Grow;
  public
    constructor Create(ItemSize, MaxItems : DWord);
    destructor Destroy; override;
    function NewItm : Pointer;
    procedure EnumerateItems(Method: TIpEnumItemsMethod);
  end;
  {$ENDIF}

  TIpHtml = class;
  
  TIpHtmlAlign = (haDefault, haLeft, haCenter, haRight, haJustify, haChar, haUnknown);
  TIpHtmlVAlign = (hvaTop, hvaMiddle, hvaBottom);
  TIpHtmlVAlign3 = (hva3Top, hva3Middle, hva3Bottom, hva3Baseline, hva3Default);

  TIpHtmlElemMarginStyle = (hemsAuto, // use default
                            hemsPx  // pixel
                            );

  TIpHtmlElemMargin = record
    Style: TIpHtmlElemMarginStyle;
    Size: single; // negative values are not yet supported
  end;


  {$IFDEF IP_LAZARUS}
  TIpAbstractHtmlDataProvider = class;
  {$DEFINE CSS_INTERFACE}
{$I ipcss.inc}
  {$UNDEF CSS_INTERFACE}
  {$ENDIF}

  TIpHtmlInteger = class(TPersistent)
  {!!.10 new - Integer property which can be scaled}
  private
    FValue : Integer;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  public
    constructor Create(AValue: Integer);
    property Value: Integer read GetValue write SetValue;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlPixelsType = (hpUndefined, hpAbsolute);
  TIpHtmlPixels = class(TPersistent)
  private
    FValue : Integer;
    FPixelsType : TIpHtmlPixelsType;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetPixelsType(const Value: TIpHtmlPixelsType);
    procedure SetValue(const Value: Integer);
  public
    property Value: Integer read GetValue write SetValue;
    property PixelsType: TIpHtmlPixelsType read FPixelsType write SetPixelsType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlLengthType = (hlUndefined, hlAbsolute, hlPercent);
  TIpHtmlLength = class(TPersistent)
  private
    FLengthValue: Integer;
    FLengthType: TIpHtmlLengthType;
    FChange: TNotifyEvent;
    procedure SetLengthType(const Value: TIpHtmlLengthType);
    procedure SetLengthValue(const Value: Integer);
    function GetLengthValue: Integer;
    procedure DoChange;
  public
    property LengthValue : Integer read GetLengthValue write SetLengthValue;
    property LengthType : TIpHtmlLengthType read FLengthType write SetLengthType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlMultiLengthType = (hmlUndefined, hmlAbsolute, hmlPercent, hmlRelative);
  TIpHtmlMultiLength = class(TPersistent)
  private
    FLengthValue : Integer;
    FLengthType : TIpHtmlMultiLengthType;
    function GetLengthValue: Integer;
  public
    property LengthValue: Integer read GetLengthValue write FLengthValue;
    property LengthType: TIpHtmlMultiLengthType read FLengthType write FLengthType;
  end;

  TIpHtmlMultiLengthList = class(TPersistent)
  private
    List: {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    function GetEntries: Integer;
    function GetValues(Index: Integer): TIpHtmlMultiLength;
  public
    constructor Create;
    destructor Destroy; override;
    property Values[Index: Integer]: TIpHtmlMultiLength read GetValues;
    procedure AddEntry(Value: TIpHtmlMultiLength);
    procedure Clear;
    property Entries: Integer read GetEntries;
  end;

  TIpHtmlRelSizeType = (hrsUnspecified, hrsAbsolute, hrsRelative);
  TIpHtmlRelSize = class(TPersistent)
  private
    FChange: TNotifyEvent;
    FSizeType : TIpHtmlRelSizeType;
    FValue : Integer;
    procedure SetSizeType(const Value: TIpHtmlRelSizeType);
    procedure SetValue(const Value: Integer);
    procedure DoChange;
  public
    property SizeType : TIpHtmlRelSizeType read FSizeType write SetSizeType;
    property Value : Integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlNode = class;
  TIpHtmlNodeBlock = class;

  TFontNameStr = string[50];
  TIpHtmlPropAFieldsRec = record
    BaseFontSize: Integer;
    FontSize: Integer;
    FontStyle: TFontStyles;
    FontName: TFontNameStr;
  end;

  {display properties that affect the font size}
  TIpHtmlPropA = class
  private
    FPropRec : TIpHtmlPropAFieldsRec;
    FUseCount: Integer;
    FKnownSizeOfSpace: TSize;
    FSizeOfSpaceKnown : Boolean;
    procedure SetBaseFontSize(const Value: Integer);
    procedure SetFontName(const Value: TFontNameStr);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
  public
    KnownSizeOfHyphen : TSize;
    tmAscent, tmDescent, tmHeight : Integer;
    property SizeOfSpaceKnown: Boolean read FSizeOfSpaceKnown;
    procedure SetKnownSizeOfSpace(const Size:TSize);
    property KnownSizeOfSpace : TSize read FKnownSizeOfSpace;
    property BaseFontSize : Integer read FPropRec.BaseFontSize write SetBaseFontSize;
    property FontName : TFontNameStr read FPropRec.FontName write SetFontName;
    property FontSize : Integer read FPropRec.FontSize write SetFontSize;
    property FontStyle : TFontStyles read FPropRec.FontStyle write SetFontStyle;
    property UseCount : Integer read FUseCount write FUseCount;
    procedure Assign(const Source: TIpHtmlPropA);
    procedure DecUse;
    procedure IncUse;
    constructor CreateCopy(Source: TIpHtmlPropA);
  end;

  TIpHtmlPropBFieldsRec = record
    FontBaseline: Integer;
    Alignment: TIpHtmlAlign;
    FontColor: TColor;
    VAlignment: TIpHtmlVAlign3;
    LinkColor : TColor;
    VLinkColor : TColor;
    ALinkColor : TColor;
    HoverColor : TColor;
    HoverBgColor : TColor;
    BgColor : TColor;
    Preformatted : Boolean;
    NoBreak : Boolean;
    ElemMarginTop: TIpHtmlElemMargin;
    ElemMarginLeft: TIpHtmlElemMargin;
    ElemMarginBottom: TIpHtmlElemMargin;
    ElemMarginRight: TIpHtmlElemMargin;
  end;

  {display properties that don't affect the font size}
  TIpHtmlPropB = class
  private
    FPropRec : TIpHtmlPropBFieldsRec;
    FUseCount: Integer;
    FOwner: TIpHtml;
  public
    property FontBaseline : Integer read FPropRec.FontBaseline write FPropRec.FontBaseline;
    property FontColor : TColor read FPropRec.FontColor write FPropRec.FontColor;
    property Alignment : TIpHtmlAlign read FPropRec.Alignment write FPropRec.Alignment;
    property VAlignment : TIpHtmlVAlign3 read FPropRec.VAlignment write FPropRec.VAlignment;
    property LinkColor : TColor read FPropRec.LinkColor write FPropRec.LinkColor;
    property VLinkColor : TColor read FPropRec.VLinkColor write FPropRec.VLinkColor;
    property ALinkColor : TColor read FPropRec.ALinkColor write FPropRec.ALinkColor;
    property HoverColor : TColor read FPropRec.HoverColor write FPropRec.HoverColor;
    property HoverBgColor : TColor read FPropRec.HoverBgColor write FPropRec.HoverBgColor;
    property BgColor : TColor read FPropRec.BgColor write FPropRec.BgColor;
    property Preformatted : Boolean read FPropRec.Preformatted write FPropRec.Preformatted;
    property NoBreak : Boolean read FPropRec.NoBreak write FPropRec.NoBreak;
    property ElemMarginTop: TIpHtmlElemMargin read FPropRec.ElemMarginTop write FPropRec.ElemMarginTop;
    property ElemMarginLeft: TIpHtmlElemMargin read FPropRec.ElemMarginLeft write FPropRec.ElemMarginLeft;
    property ElemMarginBottom: TIpHtmlElemMargin read FPropRec.ElemMarginBottom write FPropRec.ElemMarginBottom;
    property ElemMarginRight: TIpHtmlElemMargin read FPropRec.ElemMarginRight write FPropRec.ElemMarginRight;
    property UseCount : Integer read FUseCount write FUseCount;
    procedure Assign(const Source: TIpHtmlPropB);
    procedure DecUse;
    procedure IncUse;
    constructor CreateCopy(Owner: TIpHtml; Source: TIpHtmlPropB);
    constructor Create(Owner: TIpHtml);
  end;

  { TIpHtmlProps }
  
  TIpHtmlProps = class
  {-class for holding the currently active style attributes}
  private
    function GetAlignment: TIpHtmlAlign;
    function GetALinkColor: TColor;
    function GetBaseFontSize: Integer;
    function GetBgColor: TColor;
    function GetElemMarginBottom: TIpHtmlElemMargin;
    function GetElemMarginLeft: TIpHtmlElemMargin;
    function GetElemMarginRight: TIpHtmlElemMargin;
    function GetElemMarginTop: TIpHtmlElemMargin;
    function GetFontBaseline: Integer;
    function GetFontColor: TColor;
    function GetFontName: string;
    function GetFontSize: Integer;
    function GetFontStyle: TFontStyles;
    function GetLinkColor: TColor;
    function GetPreformatted: Boolean;
    function GetVAlignment: TIpHtmlVAlign3;
    function GetVLinkColor: TColor;
    function GetHoverColor: TColor;
    function GetHoverBgColor: TColor;
    procedure SetAlignment(const Value: TIpHtmlAlign);
    procedure SetALinkColor(const Value: TColor);
    procedure SetBaseFontSize(const Value: Integer);
    procedure SetBgColor(const Value: TColor);
    procedure SetElemMarginBottom(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginLeft(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginRight(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginTop(const AValue: TIpHtmlElemMargin);
    procedure SetFontBaseline(const Value: Integer);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetLinkColor(const Value: TColor);
    procedure SetPreformatted(const Value: Boolean);
    procedure SetVAlignment(const Value: TIpHtmlVAlign3);
    procedure SetVLinkColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverBgColor(const Value: TColor);
    function GetNoBreak: Boolean;
    procedure SetNoBreak(const Value: Boolean);
    procedure CopyPropARecTo(var pRec: TIpHtmlPropAFieldsRec);
    procedure CopyPropBRecTo(var pRec: TIpHtmlPropBFieldsRec);
    procedure CopyPropARecFrom(var pRec: TIpHtmlPropAFieldsRec);
    procedure CopyPropBRecFrom(var pRec: TIpHtmlPropBFieldsRec);
    procedure FindOrCreatePropA(var pRec: TIpHtmlPropAFieldsRec);
    procedure FindOrCreatePropB(var pRec: TIpHtmlPropBFieldsRec);
    procedure SetDelayCache(b: boolean);
    function getDelayCache: boolean;
  protected
    FOwner : TIpHtml;
    PropA : TIpHtmlPropA;
    PropB : TIpHtmlPropB;
    FDelayCache: integer;
    FDirtyA, FDirtyB: Boolean;
  public
    constructor Create(Owner: TIpHtml);
    destructor Destroy; override;
    procedure Assign(Source : TIpHtmlProps);
    procedure CommitCache;
    function IsEqualTo(Compare: TIpHtmlProps): Boolean;
    function AIsEqualTo(Compare: TIpHtmlProps): Boolean;
    function BIsEqualTo(Compare: TIpHtmlProps): Boolean;
    property BaseFontSize : Integer read GetBaseFontSize write SetBaseFontSize;
    property FontName : string read GetFontName write SetFontName;
    property FontSize : Integer read GetFontSize write SetFontSize;
    property FontBaseline : Integer read GetFontBaseline write SetFontBaseline;
    property FontStyle : TFontStyles read GetFontStyle write SetFontStyle;
    property FontColor : TColor read GetFontColor write SetFontColor;
    property Alignment : TIpHtmlAlign read GetAlignment write SetAlignment;
    property VAlignment : TIpHtmlVAlign3 read GetVAlignment write SetVAlignment;
    property LinkColor : TColor read GetLinkColor write SetLinkColor;
    property VLinkColor : TColor read GetVLinkColor write SetVLinkColor;
    property ALinkColor : TColor read GetALinkColor write SetALinkColor;
    property HoverColor : TColor read GetHoverColor write SetHoverColor;
    property HoverBgColor : TColor read GetHoverBgColor write SetHoverBgColor;
    property BgColor : TColor read GetBgColor write SetBgColor;
    property Preformatted : Boolean read GetPreformatted write SetPreformatted;
    property NoBreak : Boolean read GetNoBreak write SetNoBreak;
    property ElemMarginTop: TIpHtmlElemMargin read GetElemMarginTop write SetElemMarginTop;
    property ElemMarginLeft: TIpHtmlElemMargin read GetElemMarginLeft write SetElemMarginLeft;
    property ElemMarginBottom: TIpHtmlElemMargin read GetElemMarginBottom write SetElemMarginBottom;
    property ElemMarginRight: TIpHtmlElemMargin read GetElemMarginRight write SetElemMarginRight;
  public
    property DelayCache : Boolean read getDelayCache write setDelayCache;
  end;
  
  TIpHtmlNodeAlignInline = class;

  TElementType = (etWord, etObject, etSoftLF, etHardLF, etClearLeft,
    etClearRight, etClearBoth, etIndent, etOutdent, etSoftHyphen);

  TIpHtmlElement = record
    ElementType : TElementType;
    AnsiWord: string;
    IsBlank : Integer;
    SizeProp: TIpHtmlPropA;
    Size: TSize;
    WordRect2 : TRect;
    Props : TIpHtmlProps;
    Owner : TIpHtmlNode;
    {$IFDEF IP_LAZARUS}
    IsSelected: boolean;
    {$ENDIF}
  end;
  PIpHtmlElement = ^TIpHtmlElement;

  TRectMethod = procedure(const R : TRect) of object;

  TIpHtmlNodeEnumProc = procedure(Node: TIpHtmlNode; const UserData: Pointer) of object;

  {abstract base node}
  TIpHtmlNode = class(TPersistent)
  protected
    FOwner : TIpHtml;
    FParentNode : TIpHtmlNode;
    function PageRectToScreen(const Rect : TRect; var ScreenRect: TRect): Boolean;
    procedure ScreenLine(StartPoint, EndPoint: TPoint; const Width: Integer; const Color: TColor);
    procedure ScreenRect(R : TRect; const Color : TColor);
    {$IFDEF IP_LAZARUS}
    procedure ScreenFrame(R : TRect; Raised: boolean);
    {$ENDIF}
    procedure ScreenPolygon(Points : array of TPoint; const Color : TColor);
    function PagePtToScreen(const Pt: TPoint): TPoint;
    procedure Enqueue; virtual;
    procedure SetProps(const RenderProps: TIpHtmlProps); virtual;
    procedure EnqueueElement(const Entry: PIpHtmlElement); virtual;
    function ElementQueueIsEmpty: Boolean; virtual;
    procedure ReportDrawRects(M : TRectMethod); virtual;
    procedure ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod); virtual;
    procedure ReportMapRects(M : TRectMethod); virtual;
    procedure Invalidate; virtual;
    procedure InvalidateSize; virtual;
    procedure SubmitRequest; virtual;
    procedure ResetRequest; virtual;
    function GetHint: string; virtual;
    procedure CreateControl(Parent : TWinControl); virtual;
    procedure MakeVisible; virtual;
    procedure UnmarkControl; virtual;
    procedure HideUnmarkedControl; virtual;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); virtual;
    procedure AppendSelection(var S : string); virtual;
    function ExpParentWidth: Integer; virtual;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Owner : TIpHtml read FOwner;
    procedure ImageChange(NewPicture : TPicture); virtual;
    procedure GetAttributes(Target: TStrings; IncludeValues,
      IncludeBlanks: Boolean);
    procedure SetAttributeValue(const AttrName, NewValue: string);
  end;

  TIpHtmlNodeNv = class(TIpHtmlNode)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure Invalidate; override;
    procedure InvalidateSize; override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  end;

  TIpHtmlNodeMulti = class(TIpHtmlNode)
  private
    FProps: TIpHtmlProps;
    FChildren : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    function GetChildNode(Index: Integer): TIpHtmlNode;
    function GetChildCount: Integer;
  protected
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    procedure AppendSelection(var S : string); override;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property ChildCount : Integer read GetChildCount;
    property ChildNode[Index : Integer] : TIpHtmlNode read GetChildNode;
    property Props : TIpHtmlProps read FProps;
  end;

  { TIpHtmlNodeCore }

  TIpHtmlNodeCore = class(TIpHtmlNodeMulti)
  private
    {$IFDEF IP_LAZARUS}
    FInlineCSSProps: TCSSProps;  // props from the style attribute
    FCombinedCSSProps: TCSSProps; // props from all matching CSS selectors plus inline CSS combined
    FHoverPropsLookupDone: Boolean;
    FHoverPropsRef: TCSSProps; // props for :hover (this is only a cached reference, we don't own it)
    FElementName: String;
    {$ENDIF}
    FStyle: string;
    FClassId: string;
    FTitle: string;
    FId: string;
  protected
    procedure ParseBaseProps(aOwner : TIpHtml);
    {$IFDEF IP_LAZARUS}
    function SelectCSSFont(const aFont: string): string;
    procedure ApplyCSSProps(const ACSSProps: TCSSProps; const props: TIpHtmlProps);
    procedure LoadAndApplyCSSProps; virtual;
    function ElementName: String;
    function GetFontSizeFromCSS(CurrentFontSize:Integer; aFontSize: string):Integer;
    {$ENDIF}
  public
    {$IFDEF IP_LAZARUS}
    destructor Destroy; override;
    {$ENDIF}
    property ClassId : string read FClassId write FClassId;
    property Id : string read FId write FId;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
    {$IFDEF IP_LAZARUS}
    property InlineCSS: TCSSProps read FInlineCSSProps write FInlineCSSProps;
    {$ENDIF}
  end;

  TIpHtmlNodeInline = class(TIpHtmlNodeCore)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure Invalidate; override;
  end;

  TIpHtmlImageAlign = (hiaTop, hiaMiddle, hiaBottom, hiaLeft, hiaRight, hiaCenter);

  TIpHtmlNodeAlignInline = class(TIpHtmlNodeInline)
  private
    FAlignment: TIpHtmlImageAlign;
  protected
    Element : PIpHtmlElement;
    procedure Enqueue; override;
    procedure Draw(Block: TIpHtmlNodeBlock); virtual; abstract;
    procedure SetRect(TargetRect: TRect); virtual;
    function GetDim(ParentWidth: Integer): TSize; virtual; abstract;
    procedure CalcMinMaxWidth(var Min, Max: Integer); virtual; abstract;
    procedure SetAlignment(const Value: TIpHtmlImageAlign);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Align : TIpHtmlImageAlign read FAlignment write SetAlignment;
  end;

  TIpHtmlNodeControl = class(TIpHtmlNodeAlignInline)
  protected
    FControl : TWinControl;
    Shown : Boolean;
    FAlt: string;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure HideUnmarkedControl; override;
    procedure UnmarkControl; override;
    procedure AddValues(NameList, ValueList : TStringList); virtual; abstract;
    procedure Reset; virtual; abstract;
    function Successful: Boolean; virtual; abstract;
    function adjustFromCss: boolean;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Control: TWinControl read FControl;
    property Alt : string read FAlt write FAlt;
  end;


  // Used by TIpHtmlNodeBlock

  TWordInfo = record
    BaseX     : Integer;
    BOff      : Integer;
    CurAsc    : Integer;
    Sz        : TSize;
    VA        : TIpHtmlVAlign3;
    Hs        : Integer;
  end;
  PWordInfo = ^TWordInfo;

  TWordList = array[0..Pred(MAXWORDS)] of TWordInfo;
  PWordList = ^TWordList;

  { TIpHtmlNodeBlock }

  TIpHtmlNodeBlock = class(TIpHtmlNodeCore)
  private
    FLeftQueue, FRightQueue : TFPList; // TList;
    FVRemainL, FVRemainR : Integer;
    FLIdent, FRIdent : Integer;
    FTextWidth, FTotWidth : Integer;
    FFirstWord, FLastWord : Integer;
    FMaxAscent, FMaxDescent, FMaxHeight : Integer;
    FBlockAscent, FBlockDescent, FBlockHeight : Integer;
    FCurAscent, FCurDescent, FCurHeight : Integer;
    iElem, YYY : Integer;
    FBaseOffset : Integer;
    FLineBreak, FExpBreak, FCanBreak : Boolean;
    FIgnoreHardLF : Boolean;
    FTempCenter : Boolean;
    FLTrim : Boolean;
    FLastBreakpoint : Integer;
    FHyphenSpace : Integer;
    FSoftLF, FSoftBreak : Boolean;
    FAl, FSaveAl : TIpHtmlAlign;
    FVAL: TIpHtmlVAlign3;
    FWordInfo : PWordList;
    FWordInfoSize : Integer;
    FClear : (cNone, cLeft, cRight, cBoth);
    FCurProps : TIpHtmlProps;
    FxySize : TSize;
    FSizeOfSpace : TSize;
    FSizeOfHyphen : TSize;
    FCanvas: Tcanvas;
    function NextElemIsSoftLF: Boolean;
    procedure UpdSpaceHyphenSize(aProps: TIpHtmlProps);
    procedure UpdPropMetrics(aProps: TIpHtmlProps);
    function CheckSelection(aSelIndex: Integer): Boolean;
    // Used by RenderQueue :
    procedure DoRenderFont(var aCurWord: PIpHtmlElement);
    procedure DoRenderElemWord(aCurWord: PIpHtmlElement; aCurTabFocus: TIpHtmlNode);
    // Used by LayoutQueue :
    procedure QueueInit(const TargetRect: TRect);
    procedure InitMetrics;
    function QueueLeadingObjects: Integer;
    function TrimTrailingBlanks(aFirstElem: Integer = 0): Integer;
    procedure DoQueueAlign(const TargetRect: TRect; aExpLIndent: Integer);
    procedure OutputQueueLine;
    procedure DoQueueClear;
    procedure ApplyQueueProps(aCurElem: PIpHtmlElement; var aPrefor : Boolean);
    procedure DoQueueElemWord(aCurElem: PIpHtmlElement);
    function DoQueueElemObject(var aCurElem: PIpHtmlElement): boolean;
    function DoQueueElemSoftLF(const W: Integer): boolean;
    function DoQueueElemHardLF: boolean;
    function DoQueueElemClear(aCurElem: PIpHtmlElement): boolean;
    procedure DoQueueElemIndentOutdent;
    procedure DoQueueElemSoftHyphen;
    function CalcVRemain(aVRemain: integer; var aIdent: integer): integer;
    procedure SetWordInfoLength(NewLength : Integer);
    {$IFDEF IP_LAZARUS_DBG}
    procedure DumpQueue(bStart: boolean=true);
    {$ENDIF}
  protected
    FPageRect : TRect;
    FElementQueue : TFPList; // TList;
    FBlockMin, FBlockMax : Integer;
    FLastW, FLastH : Integer;
    FBackground : string;
    FBgColor : TColor;
    FTextColor : TColor;
    procedure RenderQueue;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure Render(const RenderProps: TIpHtmlProps); virtual;
    procedure Layout(const RenderProps: TIpHtmlProps; const TargetRect : TRect); virtual;
    procedure RelocateQueue(const dx, dy: Integer);
    procedure LayoutQueue(const TargetRect: TRect);
    procedure CalcMinMaxQueueWidth(var aMin, aMax: Integer);
    procedure CalcMinMaxPropWidth(const RenderProps: TIpHtmlProps; var aMin, aMax: Integer); virtual;
    procedure ClearWordList;
    procedure Invalidate; override;
    function GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
    procedure InvalidateSize; override;
    function Level0: Boolean;
    procedure ReportCurDrawRects(aOwner: TIpHtmlNode; M : TRectMethod); override;
    property PageRect : TRect read FPageRect;
    procedure AppendSelection(var S : string); override;
    procedure UpdateCurrent(Start: Integer; const CurProps : TIpHtmlProps);
    procedure SetBackground(const AValue: string);
    procedure SetBgColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    {$IFDEF IP_LAZARUS}
    procedure LoadAndApplyCSSProps; override;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Background : string read FBackground write SetBackground;
    property BgColor : TColor read FBgColor write SetBgColor;
    property TextColor : TColor read FTextColor write SetTextColor;
  end;

  TIpHtmlDirection = (hdLTR, hdRTL);
  TIpHtmlNodeHEAD = class(TIpHtmlNodeMulti)
  private
    FProfile: string;
    FLang: string;
    FDir: TIpHtmlDirection;
  public
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Lang : string read FLang write FLang;
    property Profile : string read FProfile write FProfile;
  end;

  { TIpHtmlNodeText }

  TIpHtmlNodeText = class(TIpHtmlNode)
  private
    FEscapedText : string;
    FFirstW : Boolean;
    function GetAnsiText: string;
    procedure SetAnsiText(const Value: string);
    procedure SetEscapedText(const Value: string);
    procedure AddAWord(StartP: PAnsiChar);
    function CutAndAddWord(StartP, EndP: PAnsiChar): PAnsiChar;
    procedure DoPreformattedWords(N: PAnsiChar);
    procedure DoNormalWords(N: PAnsiChar);
    procedure BuildWordList;
  protected
    PropsR : TIpHtmlProps; {reference}
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property ANSIText : string read GetAnsiText write SetAnsiText;
    property EscapedText : string read FEscapedText write SetEscapedText;
  end;

  { TIpHtmlNodeGenInline }

  TIpHtmlNodeGenInline = class(TIpHtmlNodeInline)
  protected
    Props: TIpHtmlProps;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); virtual; abstract;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlNodeFONT = class(TIpHtmlNodeGenInline)
  private
    FSize: TIpHtmlRelSize;
    FFace: string;
    FColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetFace(const Value: string);
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
    procedure SizeChanged(Sender: TObject);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Color : TColor read FColor write SetColor;
    property Face : string read FFace write SetFace;
    property Size : TIpHtmlRelSize read FSize write FSize;
  end;

  TIpHtmlNodeSTYLE = class(TIpHtmlNodeMulti)
  private
    FMedia: string;
    FTitle: string;
    {$IFDEF IP_LAZARUS}
    FType: string;
    {$ENDIF}
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
  public
    property Media : string read FMedia write FMedia;
    property Title : string read FTitle write FTitle;
    {$IFDEF IP_LAZARUS}
    property Type_ : string read FType write FType;
    {$ENDIF}
  end;

  TIpHtmlNodeSCRIPT = class(TIpHtmlNodeNv);

  TIpHtmlNodeNOSCRIPT = class(TIpHtmlNodeInline);

  TIpHtmlHeaderSize = 1..6;
  TIpHtmlNodeHeader = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
    FSize : TIpHtmlHeaderSize;
  protected
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Size : TIpHtmlHeaderSize read FSize write FSize;
  end;

  TIpHtmlNodeP = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
    procedure SetAlign(const Value: TIpHtmlAlign);
  protected
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write SetAlign;
  end;

  TIpHtmlNodeADDRESS = class(TIpHtmlNodeInline);

  TIpHtmlULType = (ulDisc, ulSquare, ulCircle);
  TIpHtmlNodeList = class(TIpHtmlNodeInline)
  private
    FCompact : Boolean;
    FListType : TIpHtmlULType;
    procedure SetListType(const Value: TIpHtmlULType);
  protected
    procedure Enqueue; override;
  public
    property Compact : Boolean read FCompact write FCompact;
    property ListType : TIpHtmlULType read FListType write SetListType;
  end;

  TIpHtmlNodeUL = class(TIpHtmlNodeList);
  TIpHtmlNodeDIR = class(TIpHtmlNodeList);
  TIpHtmlNodeMENU = class(TIpHtmlNodeList);

  TIpHtmlOLStyle = (olArabic, olLowerAlpha, olUpperAlpha, olLowerRoman, olUpperRoman);
  TIpHtmlNodeOL = class(TIpHtmlNodeInline)
  private
    FCompact : Boolean;
    FStart : Integer;
    FOLStyle : TIpHtmlOLStyle;
    procedure SetStart(const Value: Integer);
    procedure SetOLStyle(const Value: TIpHtmlOLStyle);
  protected
    Counter : Integer;
    procedure Enqueue; override;
    function GetNumString : string;
  public
    property Compact : Boolean read FCompact write FCompact;
    property Start : Integer read FStart write SetStart;
    property Style : TIpHtmlOLStyle read FOLStyle write SetOLStyle;
  end;

  TIpHtmlNodeLI = class(TIpHtmlNodeAlignInline)
  private
    FCompact: Boolean;
    FListType : TIpHtmlULType;
    FValue : Integer;
    procedure SetListType(const Value: TIpHtmlULType);
    procedure SetValue(const Value: Integer);
  protected
    WordEntry : PIpHtmlElement;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function GrossDrawRect: TRect;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    property Compact : Boolean read FCompact write FCompact;
    property ListType : TIpHtmlULType read FListType write SetListType;
    property Value : Integer read FValue write SetValue;
  end;

  TIpHtmlFormMethod = (hfmGet, hfmPost);
  TIpHtmlNodeFORM = class(TIpHtmlNodeInline)
  private
    FAccept: string;
    FAcceptCharset: string;
    FName: string;
    FEnctype: string;
    FAction: string;
    FMethod: TIpHtmlFormMethod;
  protected
    procedure AddChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetRequest; override;
    {$IFNDEF HtmlWithoutHttp}
    procedure SubmitRequest; override;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ResetForm;
    procedure SubmitForm;
    property Accept : string read FAccept write FAccept;
    property AcceptCharset : string read FAcceptCharset write FAcceptCharset;
    property Action : string read FAction write FAction;
    property Enctype : string read FEnctype write FEnctype;
    property Method : TIpHtmlFormMethod read FMethod write FMethod;
    property Name : string read FName write FName;
  end;

  TIpHtmlNodeHtml = class(TIpHtmlNodeMulti)
  private
    FLang: string;
    FVersion: string;
    FDir: TIpHtmlDirection;
  protected
    function HasBodyNode : Boolean;
    procedure Render(const RenderProps: TIpHtmlProps);
    procedure CalcMinMaxHtmlWidth(const RenderProps: TIpHtmlProps; var Min, Max: Integer);
    function GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
    procedure Layout(const RenderProps: TIpHtmlProps; const TargetRect : TRect);
  public
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Lang : string read FLang write FLang;
    property Version : string read FVersion write FVersion;
  end;

  TIpHtmlNodeTITLE = class(TIpHtmlNodeNv)
  private
    FTitle: string;
  public
    property Title : string read FTitle write FTitle;
  end;

  { TIpHtmlNodeBODY }

  TIpHtmlNodeBODY = class(TIpHtmlNodeBlock)
  private
    FLink : TColor;
    FVLink : TColor;
    FALink : TColor;
    procedure SetAlink(const Value: TColor);
    procedure SetLink(const Value: TColor);
    procedure SetVlink(const Value: TColor);
  protected
    BGPicture : TPicture;
    procedure Render(const RenderProps: TIpHtmlProps); override;
    {$IFDEF IP_LAZARUS}
    procedure LoadAndApplyCSSProps; override;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;
    property ALink : TColor read Falink write SetAlink;
    property Link : TColor read FLink write SetLink;
    property VLink : TColor read FVLink write SetVlink;
  end;

  TIpHtmlNodeNOFRAMES = class(TIpHtmlNodeCore);

  TIpHtmlNodeFRAMESET = class(TIpHtmlNodeCore)
  private
    FCols: TIpHtmlMultiLengthList;
    FRows: TIpHtmlMultiLengthList;
  public
    destructor Destroy; override;
    property Cols : TIpHtmlMultiLengthList read FCols write FCols;
    property Rows : TIpHtmlMultiLengthList read FRows write FRows;
  end;

  TIpHtmlFrameScrolling = (hfsAuto, hfsYes, hfsNo);
  TIpHtmlNodeFRAME = class(TIpHtmlNodeCore)
  private
    FFrameBorder: Integer;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FNoResize: Boolean;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  public
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property NoResize : Boolean read FNoResize write FNoResize;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
  end;

  TIpHtmlFrame = class;

  TIpHtmlNodeIFRAME = class(TIpHtmlNodeControl)
  private
    FAlign: TIpHtmlAlign;
    FFrameBorder: Integer;
    FHeight: TIpHtmlLength;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    FWidth: TIpHtmlLength;
    FFrame : TIpHtmlFrame;
    procedure SetAlign(const Value: TIpHtmlAlign);
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  protected
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    procedure WidthChanged(Sender: TObject);
  public
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write SetAlign;
    property Frame: TIpHtmlFrame read FFrame;
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property Height : TIpHtmlLength read FHeight write FHeight;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeDL = class(TIpHtmlNodeInline)
  private
    FCompact : Boolean;
  protected
    procedure Enqueue; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    property Compact : Boolean read FCompact write FCompact;
  end;

  TIpHtmlNodeDT = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
  end;

  TIpHtmlNodeDD = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
  end;

  TIpHtmlNodePRE = class(TIpHtmlNodeInline)
  private
  protected
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlNodeDIV = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
  protected
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write FAlign;
  end;

  { TIpHtmlNodeSPAN }

  TIpHtmlNodeSPAN = class(TIpHtmlNodeGenInline)
  private
    FAlign : TIpHtmlAlign;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    property Align : TIpHtmlAlign read FAlign write FAlign;
  end;

  TIpHtmlNodeBLINK = class(TIpHtmlNodeInline);

  TIpHtmlNodeBLOCKQUOTE = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  end;

  TIpHtmlNodeQ = class(TIpHtmlNodeInline);

  TIpHtmlNodeINS = class(TIpHtmlNodeGenInline)
  private
    FCite: string;
    FDateTime: string;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    property Cite : string read FCite write FCite;
    property DateTime : string read FDateTime write FDateTime;
  end;

  TIpHtmlNodeDEL = class(TIpHtmlNodeGenInline)
  private
    FCite: string;
    FDateTime: string;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    property Cite : string read FCite write FCite;
    property DateTime : string read FDateTime write FDateTime;
  end;

  TIpHtmlFontStyles = (hfsTT, hfsI, hfsB, hfsU, hfsSTRIKE, hfsS,
   hfsBIG, hfsSMALL, hfsSUB, hfsSUP);
  TIpHtmlNodeFontStyle = class(TIpHtmlNodeGenInline)
  private
    FHFStyle : TIpHtmlFontStyles;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    property Style : TIpHtmlFontStyles read FHFStyle write FHFStyle;
  end;

  TIpHtmlPhraseStyle = (hpsEM, hpsSTRONG, hpsDFN, hpsCODE, hpsSAMP,
    hpsKBD, hpsVAR, hpsCITE, hpsABBR, hpsACRONYM);
  TIpHtmlNodePhrase = class(TIpHtmlNodeGenInline)
  private
    FPhrStyle : TIpHtmlPhraseStyle;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    property Style : TIpHtmlPhraseStyle read FPhrStyle write FPhrStyle;
  end;

  TIpHtmlNodeHR = class(TIpHtmlNodeAlignInline)
  private
    FColor: TColor;
    FNoShade : Boolean;
    FSize : TIpHtmlInteger;
    FWidth : TIpHtmlLength;
  protected
    SizeWidth : TIpHtmlPixels;
    FDim : TSize;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    function GrossDrawRect: TRect;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure Enqueue; override;
    procedure WidthChanged(Sender: TObject);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Color : TColor read FColor write FColor;
    property NoShade  : Boolean read FNoShade write FNoShade;
    property Size : TIpHtmlInteger read FSize write FSize;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlBreakClear = (hbcNone, hbcLeft, hbcRight, hbcAll);

  { TIpHtmlNodeBR }

  TIpHtmlNodeBR = class(TIpHtmlNodeInline)
  private
    FClear: TIpHtmlBreakClear;
    FId: string;
  protected
    procedure Enqueue; override;
    procedure SetClear(const Value: TIpHtmlBreakClear);
  public
    constructor Create(ParentNode: TIpHtmlNode);
    property Clear : TIpHtmlBreakClear read FClear write SetClear;
    property Id : string read FId write FId;
  end;

  TIpHtmlNodeNOBR = class(TIpHtmlNodeGenInline)
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  end;

  TIpHtmlMapShape = (hmsDefault, hmsRect, hmsCircle, hmsPoly);
  TIpHtmlNodeA = class(TIpHtmlNodeInline)
  private
    FHRef: string;
    FName: string;
    FRel: string;
    FRev: string;
    FShape: TIpHtmlMapShape;
    FTabIndex: Integer;
    FTarget: string;
    procedure SetHRef(const Value: string);
    procedure SetName(const Value: string);
  protected
    AreaList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    FHasRef : Boolean;
    FHot: Boolean;
    MapAreaList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    procedure ClearAreaList;
    function PtInRects(const P : TPoint) : Boolean;
    function RelMapPoint(const P: TPoint): TPoint;
    procedure SetHot(const Value: Boolean);
    procedure AddArea(const R: TRect);
    procedure BuildAreaList;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure AddMapArea(const R: TRect);
    function GetHint: string; override;
    procedure DoOnFocus;
    procedure DoOnBlur;
    property HasRef : Boolean read FHasRef;
    property Hot : Boolean read FHot write SetHot;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure MakeVisible; override;
    property HRef : string read FHRef write SetHRef;
    property Name : string read FName write SetName;
    property Rel : string read FRel write FRel;
    property Rev : string read FRev write FRev;
    property Shape : TIpHtmlMapShape read FShape write FShape;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeIMG = class(TIpHtmlNodeAlignInline)
  private
    FAlt: string;
    FBorder: Integer;
    FHeight: TIpHtmlPixels;
    FHSpace: Integer;
    FIsMap: Boolean;
    FLongDesc: string;
    FName: string;
    FPicture : TPicture;
    FSrc: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
    {$IFDEF IP_LAZARUS}
    function GetBorder: Integer;
    {$ENDIF}
    procedure SetBorder(const Value: Integer);
    procedure SetUseMap(const Value: string);
    procedure SetHSpace(const Value: Integer);
    procedure SetVSpace(const Value: Integer);
  protected
    FSize : TSize;
    NetDrawRect : TRect;
    SizeWidth : TIpHtmlPixels;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    procedure LoadImage;
    procedure UnloadImage;
    function GrossDrawRect: TRect;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    function GetHint: string; override;
    procedure DimChanged(Sender: TObject);
    procedure InvalidateSize; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;
    property Alt : string read FAlt write FAlt;
    {$IFDEF IP_LAZARUS}
    property Border : Integer read GetBorder write SetBorder;
    {$ELSE}
    property Border : Integer read FBorder write SetBorder;
    {$ENDIF}
    property Height : TIpHtmlPixels read FHeight write FHeight;
    property HSpace : Integer read FHSpace write SetHSpace;
    property IsMap : Boolean read FIsMap write FIsMap;
    property LongDesc : string read FLongDesc write FLongDesc;
    property Name : string read FName write FName;
    property Picture : TPicture read FPicture;
    property Src : string read FSrc write FSrc;
    property UseMap : string read FUseMap write SetUseMap;
    property VSpace : Integer read FVSpace write SetVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeAPPLET = class(TIpHtmlNodeInline)
  private
    FArchive: string;
    FObjectCode: string;
    FVSpace: Integer;
    FHSpace: Integer;
    FHeight: Integer;
    FWidth: TIpHtmlLength;
    FName: string;
    FCodebase: string;
    FCode: string;
    FAlt: string;
    FAlignment: TIpHtmlImageAlign;
  protected
    function GetHint: string; override;
    procedure WidthChanged(Sender: TObject);
  public
    destructor Destroy; override;
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Alt : string read FAlt write FAlt;
    property Code : string read FCode write FCode;
    property Codebase : string read FCodebase write FCodebase;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Name : string read FName write FName;
    property ObjectCode : string read FObjectCode write FObjectCode;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeOBJECT = class(TIpHtmlNodeInline)
  private
    FAlignment: TIpHtmlImageAlign;
    FArchive: string;
    FBorder: Integer;
    FCodebase: string;
    FCodeType: string;
    FData: string;
    FDeclare: Boolean;
    FHeight: Integer;
    FHSpace: Integer;
    FName: string;
    FStandby: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
  protected
    procedure WidthChanged(Sender: TObject);
  public
    destructor Destroy; override;
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Border : Integer read FBorder write FBorder;
    property Codebase : string read FCodebase write FCodebase;
    property CodeType : string read FCodeType write FCodeType;
    property Data : string read FData write FData;
    property Declare : Boolean read FDeclare write FDeclare;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Name : string read FName write FName;
    property Standby : string read FStandby write FStandby;
    property UseMap : string read FUseMap write FUseMap;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlObjectValueType = (hovtData, hovtRef, hovtObject);
  TIpHtmlNodePARAM = class(TIpHtmlNodeNv)
  private
    FId: string;
    FValueType: TIpHtmlObjectValueType;
    FValue: string;
    FName: string;
  public
    property Id : string read FId write FId;
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
    property ValueType : TIpHtmlObjectValueType read FValueType write FValueType;
  end;

  TIpHtmlNodeBASEFONT = class(TIpHtmlNodeGenInline)
  private
    FSize: Integer;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    property Size : Integer read FSize write FSize;
  end;

  TIpHtmlNodeMAP = class(TIpHtmlNodeCore)
  private
    FName : string;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Name : string read FName write FName;
  end;

  TIpHtmlNodeAREA = class(TIpHtmlNodeCore)
  private
    FShape: TIpHtmlMapShape;
    FTabIndex: Integer;
    FTarget: string;
  protected
    FNoHRef: Boolean;
    FHRef: string;
    FCoords: string;
    FAlt: string;
    FRect : TRect;
    FRgn : HRgn;
    procedure Reset;
    function GetHint: string; override;
    function PtInRects(const P : TPoint) : Boolean;
  public
    destructor Destroy; override;
    {$IF DEFINED(CBuilder) OR DEFINED(IP_LAZARUS)}
    property Rect : TRect read FRect;
    {$ENDIF}
    property Alt : string read FAlt write FAlt;
    property Coords : string read FCoords write FCoords;
    property HRef : string read FHRef write FHRef;
    property NoHRef : Boolean read FNoHRef write FNoHRef;
    {$IF NOT (DEFINED(CBuilder) OR DEFINED(IP_LAZARUS))}
    property Rect : TRect read FRect;
    {$ENDIF}
    property Shape : TIpHtmlMapShape read FShape write FShape;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeMETA = class(TIpHtmlNodeNv)
  private
    FScheme: string;
    FContent: string;
    FHttpEquiv: string;
    FName: string;
  public
    property Content : string read FContent write FContent;
    property HttpEquiv: string read FHttpEquiv write FHttpEquiv;
    property Name : string read FName write FName;
    property Scheme : string read FScheme write FScheme;
  end;

  TIpHtmlNodeLINK = class(TIpHtmlNodeCore)
  private
    FHRef: string;
    FRev: string;
    FRel: string;
    {$IFDEF IP_LAZARUS}
    FType: string;
    {$ENDIF}
  public
    property HRef : string read FHRef write FHRef;
    property Rel : string read FRel write FRel;
    property Rev : string read FRev write FRev;
    {$IFDEF IP_LAZARUS}
    property Type_ : string read FType write FType;
    {$ENDIF}
  end;

  TIpHtmlVAlignment2 = (hva2Top, hva2Bottom, hva2Left, hva2Right);

  { TIpHtmlNodeCAPTION }

  TIpHtmlNodeCAPTION = class(TIpHtmlNodeBlock)
  private
    FAlign: TIpHtmlVAlignment2;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    property Align : TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TIpHtmlFrameProp = (hfVoid, hfAbove, hfBelow, hfHSides, hfLhs, hfRhs,
    hfvSides, hfBox, hfBorder);

  TIpHtmlRules = (hrNone, hrGroups, hrRows, hrCols, hrAll);

  {TIntArr = array [0..Pred(MAXINTS)] of Integer;}
  TInternalIntArr = array [0..Pred(MAXINTS)] of Integer;
  PInternalIntArr = ^TInternalIntArr;
  TIntArr = class
  private
    InternalIntArr : PInternalIntArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index, Value: Integer);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: Integer read GetValue write SetValue; default;
  end;

  TInternalRectArr = array [0..Pred(MAXINTS)] of PRect;
  PInternalRectArr = ^TInternalRectArr;
  TRectArr = class
  private
    InternalRectArr : PInternalRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): PRect;
    procedure SetValue(Index: Integer; Value: PRect);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: PRect read GetValue write SetValue; default;
  end;

  TInternalRectRectArr = array [0..Pred(MAXINTS)] of TRectArr;
  PInternalRectRectArr = ^TInternalRectRectArr;
  TRectRectArr = class
  protected
    InternalRectRectArr : PInternalRectRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): TRectArr;
  public
    destructor Destroy; override;
    property Value[Index: Integer]: TRectArr read GetValue; default;
    procedure Delete(Index: Integer);
  end;

  { TIpHtmlNodeTABLE }

  TIpHtmlNodeTABLE = class(TIpHtmlNodeAlignInline)
  private
    FBgColor: TColor;
    FBorder: Integer;
    FBorderColor: TColor;
    FBorderStyle: TCSSBorderStyle;
    FCellSpacing: Integer;
    FCellPadding: Integer;
    FFrame: TIpHtmlFrameProp;
    FRules: TIpHtmlRules;
    FSummary: string;
    FTableWidth: Integer;
    procedure SetBorder(const Value: Integer);
    procedure SetCellPadding(const Value: Integer);
    procedure SetCellSpacing(const Value: Integer);
    procedure SetFrame(const Value: TIpHtmlFrameProp);
    procedure SetRules(const Value: TIpHtmlRules);
  protected
    FWidth: TIpHtmlLength;
    CellOverhead, {sum of col widths + CellOverhead = TableWidth}
    FColCount : Integer;
    ColTextWidth : TIntArr; {actual column widths}
    ColStart : TIntArr; {start of each column relative to table's left}
    ColTextWidthMin,
    ColTextWidthMax : TIntArr; {min and max column widths}
    RowSp : TIntArr; {dynamic flag used for row spanning}
    FCaption : TIpHtmlNodeCAPTION;
    BorderRect : TRect;
    BorderRect2 : TRect; {includes caption if any}
    RUH, RUV : Integer; {ruler width hor/vert}
    BL, BR, BT, BB : Integer; {border width, left, right, top, bottom}
    {$IFNDEF IP_LAZARUS}
    CS2 : Integer; {cell space div 2}
    {$ENDIF}
    SizeWidth : TIpHtmlPixels; {last computed width of table}
    FMin, FMax : Integer;
    procedure CalcMinMaxColTableWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer);
    procedure CalcSize(const ParentWidth: Integer; const RenderProps: TIpHtmlProps);
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SetRect(TargetRect: TRect); override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure InvalidateSize; override;
    function GetColCount: Integer;
    procedure Enqueue; override;
    property ColCount : Integer read GetColCount;
    procedure WidthChanged(Sender: TObject);
    function ExpParentWidth: Integer; override;
    {$IFDEF IP_LAZARUS}
    procedure LoadAndApplyCSSProps; override;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property BgColor : TColor read FBgColor write FBgColor;
    property Border : Integer read FBorder write SetBorder;
    property BorderStyle: TCSSBorderStyle read FBorderStyle write FBorderStyle;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property CalcMinWidth: Integer read FMin;
    property CalcMaxWidth: Integer read FMax;
    property CalcTableWidth: Integer read FTableWidth;
    property CellPadding : Integer read FCellPadding write SetCellPadding;
    property CellSpacing : Integer read FCellSpacing write SetCellSpacing;
    property Frame : TIpHtmlFrameProp read FFrame write SetFrame;
    property Rules : TIpHtmlRules read FRules write SetRules;
    property Summary : string read FSummary write FSummary;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeTHeadFootBody = class(TIpHtmlNodeCore);

  TIpHtmlNodeTABLEHEADFOOTBODYClass = class of TIpHtmlNodeTHeadFootBody;

  TIpHtmlNodeTHEAD = class(TIpHtmlNodeTHeadFootBody)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeTFOOT = class(TIpHtmlNodeTHeadFootBody)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  public
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeTBODY = class(TIpHtmlNodeTHeadFootBody)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeCOLGROUP = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
    FSpan: Integer;
    FVAlign: TIpHtmlVAlign3;
    FWidth: TIpHtmlMultiLength;
  public
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Span : Integer read FSpan write FSpan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlMultiLength read FWidth write FWidth;
  end;

  TIpHtmlNodeCOL = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
    FSpan: Integer;
    FWidth: TIpHtmlMultiLength;
  public
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Span : Integer read FSpan write FSpan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlMultiLength read FWidth write FWidth;
  end;

  { TIpHtmlNodeTR }

  TIpHtmlNodeTR = class(TIpHtmlNodeBlock)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign;
  protected
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign read FVAlign write FVAlign;
  end;

  TIpHtmlCellScope = (hcsUnspec, hcsRow, hcsCol, hcsRowGroup, hcsColGroup);

  { TIpHtmlNodeTableHeaderOrCell }

  TIpHtmlNodeTableHeaderOrCell = class(TIpHtmlNodeBlock)
  private
    FAlign: TIpHtmlAlign;
    FCalcWidthMin: Integer;
    FCalcWidthMax: Integer;
    FColspan: Integer;
    FHeight: TIpHtmlPixels;
    FNowrap: Boolean;
    FRowspan: Integer;
    FWidth: TIpHtmlLength;
    FVAlign: TIpHtmlVAlign3;
  protected
    FPadRect : TRect;
    procedure Render(const RenderProps: TIpHtmlProps); override;
    procedure Layout(const RenderProps: TIpHtmlProps; const TargetRect : TRect); override;
    procedure CalcMinMaxPropWidth(const RenderProps: TIpHtmlProps; var Min, Max: Integer); override;
    property PadRect : TRect read FPadRect;
    procedure DimChanged(Sender: TObject);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property CalcWidthMin: Integer read FCalcWidthMin;
    property CalcWidthMax: Integer read FCalcWidthMax;
    property Colspan : Integer read FColspan write FColspan;
    property Height : TIpHtmlPixels{Integer} read FHeight write FHeight;
    property Nowrap : Boolean read FNowrap write FNowrap;
    property Rowspan : Integer read FRowspan write FRowspan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  { TIpHtmlNodeTH }

  TIpHtmlNodeTH = class(TIpHtmlNodeTableHeaderOrCell)
  public
    constructor Create(ParentNode: TIpHtmlNode);
  end;

  { TIpHtmlNodeTD }

  TIpHtmlNodeTD = class(TIpHtmlNodeTableHeaderOrCell)
  public
    constructor Create(ParentNode: TIpHtmlNode);
  end;

  TIpHtmlInputType = (hitText, hitPassword, hitCheckbox, hitRadio,
    hitSubmit, hitReset, hitFile, hitHidden, hitImage, hitButton);

  TIpHtmlNodeINPUT = class(TIpHtmlNodeControl)
  private
    FChecked: Boolean;
    FDisabled: Boolean;
    FInputType: TIpHtmlInputType;
    FMaxLength: Integer;
    FName: string;
    FReadOnly: Boolean;
    FTabIndex: Integer;
    FSize: Integer;
    FSrc: string;
    FValue: string;
  protected
    FPicture : TPicture;
    FFileEdit : TEdit;
    FFileSelect : TButton;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FileSelect(Sender: TObject);
    procedure getControlValue;
    procedure ButtonClick(Sender: TObject);
    procedure ControlOnEditingDone(Sender: TObject);
    procedure ControlOnChange(Sender: TObject);
    function GetHint: string; override;
    procedure SetImageGlyph(Picture: TPicture);
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Reset; override;
    procedure ImageChange(NewPicture : TPicture); override;
    property Checked : Boolean read FChecked write FChecked;
    property Disabled : Boolean read FDisabled write FDisabled;
    property InputType : TIpHtmlInputType read FInputType write FInputType;
    property MaxLength : Integer read FMaxLength write FMaxLength;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Size : Integer read FSize write FSize;
    property Src : string read FSrc write FSrc;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlButtonType = (hbtSubmit, hbtReset, hbtButton);

  TIpHtmlNodeBUTTON = class(TIpHtmlNodeControl)
  private
    FDisabled: Boolean;
    FTabIndex: Integer;
    FValue: string;
    FName: string;
    FInputType: TIpHtmlButtonType;
  protected
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure CreateControl(Parent : TWinControl); override;
    procedure Reset; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property ButtonType : TIpHtmlButtonType read FInputType write FInputType;
    property Disabled : Boolean read FDisabled write FDisabled;
    property Name : string read FName write FName;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlNodeSELECT = class(TIpHtmlNodeControl)
  private
    FDisabled: Boolean;
    FMultiple: Boolean;
    FComboBox: Boolean;
    FName: string;
    FSize: Integer;
    FWidth: integer;
    FTabIndex: Integer;
  protected
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure Reset; override;
    procedure ButtonClick(Sender: TObject);
    procedure ControlOnEditingDone(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure setText(aText: string);
    function getText: string;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    property Disabled : Boolean read FDisabled write FDisabled;
    property Multiple : Boolean read FMultiple write FMultiple;
    property ComboBox : Boolean read FComboBox write FComboBox;
    property Name : string read FName write FName;
    property Size : Integer read FSize write FSize;
    property Width : Integer read FWidth write FWidth;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Text : string read getText write setText;
  end;

  TIpHtmlNodeOPTION = class(TIpHtmlNodeCore)
  private
    FDisabled: Boolean;
    FOptionLabel: string;
    FSelected: Boolean;
    FValue: string;
  public
    property Disabled : Boolean read FDisabled write FDisabled;
    property OptionLabel : string read FOptionLabel write FOptionLabel;
    property Selected : Boolean read FSelected write FSelected;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlNodeOPTGROUP = class(TIpHtmlNodeCore)
  private
    FDisabled: Boolean;
    FGroupLabel: string;
  public
    property Disabled : Boolean read FDisabled write FDisabled;
    property GroupLabel : string read FGroupLabel write FGroupLabel;
  end;

  TIpHtmlNodeTEXTAREA = class(TIpHtmlNodeControl)
  private
    FDisabled: Boolean;
    FReadOnly: Boolean;
    FTabIndex: Integer;
    FCols: Integer;
    FRows: Integer;
    FName: string;
  protected
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    procedure ControlOnEditingDone(Sender: TObject);
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    property Cols : Integer read FCols write FCols;
    property Disabled : Boolean read FDisabled write FDisabled;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Rows : Integer read FRows write FRows;
    property TabIndex : Integer read FTabIndex write FTabIndex;
  end;

  TInvalidateEvent = procedure(Sender : TIpHtml; const Rect : TRect) of object;

  TIpHtmlNodeLABEL = class(TIpHtmlNodeInline)
  private
    FLabelFor: string;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    property LabelFor : string read FLabelFor write FLabelFor;
  end;

  TIpHtmlNodeFIELDSET = class(TIpHtmlNodeCore);

  TIpHtmlNodeLEGEND = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlVAlignment2;
  public
    property Align : TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TWriteCharProvider = procedure(C : AnsiChar) of object;

  TIpHtmlDataGetImageEvent =
    procedure(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture) of object;

  TIpHtmlScrollEvent =
    procedure(Sender: TIpHtml; const R: TRect{$IFDEF IP_LAZARUS}; ShowAtTop: Boolean{$ENDIF}) of object;

  TGetEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TPostEvent =
    procedure(Sender: TIpHtml; const URL: string; FormData: TIpFormDataEntity) of object;

  TIFrameCreateEvent =
    procedure(Sender: TIpHtml; Parent: TWinControl; Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl) of object;

  TURLCheckEvent =
    procedure(Sender: TIpHtml; const URL: string; var Visited: Boolean) of object;

  TReportURLEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TIpHtmlRectListEntry = record
    Rect : TRect;
    Node : PIpHtmlElement;
    Block : TIpHtmlNodeBlock;
  end;
  PIpHtmlRectListEntry = ^TIpHtmlRectListEntry;

  TControlEvent = procedure(Sender: TIpHtml; Node: TIpHtmlNodeControl)
    of object;

  TControlEvent2 = procedure(Sender: TIpHtml; Node: TIpHtmlNodeControl; var cancel: boolean)
    of object;

  TIpHtml = class
  private
    FHotNode : TIpHtmlNode;
    FCurElement : PIpHtmlElement;
    FHotPoint : TPoint;
    FMouseLastPoint : TPoint;
    FOnInvalidateRect : TInvalidateEvent;
    FTarget : TCanvas;
    FVLinkColor: TColor;
    FLinkColor: TColor;
    FALinkColor: TColor;
    FTextColor: TColor;
    FBgColor: TColor; //JMN
    FFactBAParag: Real;
    FHasFrames : Boolean;
    FOnGetImageX : TIpHtmlDataGetImageEvent;
    FOnScroll : TIpHtmlScrollEvent;
    FOnInvalidateSize : TNotifyEvent;
    FOnGet: TGetEvent;
    FOnPost: TPostEvent;
    FOnIFrameCreate : TIFrameCreateEvent;
    FOnURLCheck: TURLCheckEvent;
    FOnReportURL: TReportURLEvent;
    FControlClick : TControlEvent;
    FControlClick2 : TControlEvent2;
    FControlOnEditingDone : TControlEvent;
    FControlOnChange : TControlEvent;
    FControlCreate : TControlEvent;
    FCurFrameSet : TIpHtmlNodeFRAMESET;
    FCanPaint : Boolean;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    {$IFDEF IP_LAZARUS}
    FCSS: TCSSGlobalProps;
    FDocCharset: string;
    FHasBOM: boolean;
    FTabList: TIpHtmlTabList;
    {$ENDIF}
  protected
    CharStream : TStream;
    CurToken : TIpHtmlToken;
    ParmValueArray : array[TIpHtmlAttributesSet] of string;
    FHtml : TIpHtmlNodeHtml;
    CharStack : array [0..7] of AnsiChar;
    LastWasSpace: Boolean;
    LastWasClose: Boolean;
    CharSP : Integer;
    FFlagErrors : Boolean;
    IndexPhrase : string;
    TokenBuffer : TIpHtmlToken;
    FPageRect : TRect;
    HaveToken : Boolean;
    PageViewRect : TRect; {the current section of the page}
    ClientRect : TRect;   {the coordinates of the paint rectangle}
    DefaultProps : TIpHtmlProps;
    Body : TIpHtmlNodeBODY;
    FTitleNode : TIpHtmlNodeTITLE;
   {$IFDEF IP_LAZARUS}
      FDataProvider: TIpAbstractHtmlDataProvider;
      {$IFDEF UseGifImageUnit}
      GifImages : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
      {$ELSE}
      AnimationFrames : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
      {$ENDIF}
   {$ELSE}
    GifImages : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    OtherImages: {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
   {$ENDIF}
    FLIndent, FLOutdent : PIpHtmlElement;
    SoftLF,
    HardLF, HardLFClearLeft, SoftHyphen,
    HardLFClearRight, HardLFClearBoth : PIpHtmlElement;
    NameList : TStringList;
    GifQueue : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    InPre : Integer;
    InBlock : Integer;
    MapList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    AreaList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    DefaultImage : TPicture;
    MapImgList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    GlobalPos, LineNumber, LineOffset : Integer;
    PaintBufferBitmap : TBitmap;
    PaintBuffer : TCanvas;
    TokenStringBuf : PChar; {array[16383] of AnsiChar;}
    TBW : Integer;
    Destroying : Boolean;
    AllSelected : Boolean;
    RectList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    FStartSel, FEndSel : TPoint;
    ElementPool : TIpHtmlPoolManager;

    AnchorList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    FControlList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    FCURURL : string;
    DoneLoading : Boolean;
    ListLevel : Integer;

    PropACache : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    PropBCache : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
    DummyA : TIpHtmlPropA;
    DummyB : TIpHtmlPropB;

    RenderCanvas : TCanvas;
    PageHeight : Integer;
    StartPos : Integer;
    FFixedTypeface: string;
    FDefaultTypeFace: string;
    FDefaultFontSize: integer;
    ParmBuf: PChar;
    ParmBufSize: Integer;
    procedure ResetCanvasData;
    procedure ResetCache;
    procedure ResetWordLists;
    procedure ResetBlocks(Node: TIpHtmlNode);
    procedure ResetImages(Node: TIpHtmlNode);
    procedure ResetElementMetrics(P: Pointer);
    function FindPropARec(var pRec: TIpHtmlPropAFieldsRec): TIpHtmlPropA;
    procedure DelDuplicatePropA(aProp: TIpHtmlPropA);
    function FindPropBRec(var pRec: TIpHtmlPropBFieldsRec): TIpHtmlPropB;
    procedure DelDuplicatePropB(aProp: TIpHtmlPropB);
    procedure ClearCache;
    function CheckKnownURL(URL: string): boolean;
    procedure ReportReference(URL: string);
    procedure PaintSelection;
    function PageRectToScreen(const Rect: TRect; var ScreenRect: TRect): Boolean;
    function IsWhiteSpace: Boolean;
    function GetTokenString: string;
    procedure ReportError(const ErrorMsg: string);
    procedure ReportExpectedError(const ErrorMsg: string);
    procedure ReportExpectedToken(const Token: TIpHtmlToken);
    procedure EnsureClosure(const EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    function NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
    function BuildStandardEntry(EType: TElementType): PIpHtmlElement;
    function ParseDir: TIpHtmlDirection;
    procedure ParseSPAN(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseQ(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseINS(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDEL(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableBody(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableRows(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseColGroup(Parent: TIpHtmlNode);
    function ParseFrameScrollingProp: TIpHtmlFrameScrolling;
    function ParseObjectValueType: TIpHtmlObjectValueType;
    procedure ParseFrameSet(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFrame(Parent : TIpHtmlNode);
    procedure ParseIFrame(Parent : TIpHtmlNode);
    procedure ParseNOFRAMES(Parent : TIpHtmlNode);
    function ParseButtonType: TIpHtmlButtonType;
    procedure ParseNoscript(Parent: TIpHtmlNode);
    procedure ParseLEFT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBLINK(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseRIGHT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure PutToken(Token: TIpHtmlToken);
    procedure ParseParagraph(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseListItems(Parent : TIpHtmlNodeCore;
        EndToken: TIpHtmlToken; const EndTokens : TIpHtmlTokenSet;
        DefaultListStyle : TIpHtmlULType);
    procedure ParseUnorderedList(Parent: TIpHtmlNode;
          EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParseOrderedList(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseDefinitionList(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDefListItems(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
    procedure ParsePre(ParentNode : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDIV(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseCENTER(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBLOCKQUOTE(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseHR(Parent: TIpHtmlNode);
    procedure ParseFontStyle(Parent: TIpHtmlNode;
      StartToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParsePhraseElement(Parent: TIpHtmlNode;
      StartToken, EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParseAnchor(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseIMG(Parent : TIpHtmlNode);
    procedure ParseApplet(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseOBJECT(Parent : TIpHtmlNode);
    procedure ParseBasefont(Parent: TIpHtmlNode);
    procedure ParseBR(Parent : TIpHtmlNode);
    procedure ParseNOBR(Parent: TIpHtmlNode);
    procedure ParseMAP(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTABLE(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
    function FindAttribute(const AttrNameSet: TIpHtmlAttributesSet): string;
    function ColorFromString(S: string): TColor;
    function ParseAlignment: TIpHtmlAlign;
    function ParseCellAlign(Default : TIpHtmlAlign) : TIpHtmlAlign;
    function ParseFrameProp(Default: TIpHtmlFrameProp) : TIpHtmlFrameProp;
    function ParseRules(Default : TIpHtmlRules) : TIpHtmlRules;
    function ParseULStyle(Default : TIpHtmlULType): TIpHtmlULType;
    function ParseBoolean(const AttrNameSet: TIpHtmlAttributesSet): Boolean;
    function ParseInteger(const AttrNameSet: TIpHtmlAttributesSet;
      aDefault : Integer): Integer;
    function ParseHtmlInteger2(const AttrNameSet: TIpHtmlAttributesSet;
      aDefault: Integer): TIpHtmlInteger;
    function ParsePixels(const AttrNameSet: TIpHtmlAttributesSet;
             const aDefault: string): TIpHtmlPixels;
    function ParseHyperLength(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlLength;
    function ParseHyperMultiLength(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlMultiLength;
    function ParseHyperMultiLengthList(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlMultiLengthList;
    function ParseOLStyle(Default: TIpHtmlOLStyle): TIpHtmlOLStyle;
    function ParseImageAlignment(aDefault: TIpHtmlImageAlign): TIpHtmlImageAlign;
    function ParseVAlignment : TIpHtmlVAlign;
    function ParseVAlignment2 : TIpHtmlVAlignment2;
    function ParseVAlignment3 : TIpHtmlVAlign3;
    function ParseRelSize{(const Default: string)}: TIpHtmlRelSize;
    function ParseBRClear: TIpHtmlBreakClear;
    function ParseShape: TIpHtmlMapShape;
    function NextChar : AnsiChar;
    procedure Parse;
    {procedure ParseDocType;}
    procedure ParseHtml;
    function GetChar: AnsiChar;
    procedure ClearParmValueArray;
    procedure ParmValueArrayAdd(const sName, sValue: string);
    function HtmlTokenListIndexOf(const TokenString: PAnsiChar): integer;
    procedure NextToken;
    procedure PutChar(Ch: AnsiChar);
    procedure ParseHead(Parent : TIpHtmlNode);
    procedure ParseHeadItems(Parent : TIpHtmlNode);
    procedure ParseTitle(Parent: TIpHtmlNode);
    procedure ParseScript(Parent : TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseStyle(ParentNode : TIpHtmlNode);
    procedure ParseIsIndex;
    procedure ParseBase;
    procedure ParseLink(Parent : TIpHtmlNode);
    procedure ParseMeta(Parent : TIpHtmlNode);
    procedure ParseBody(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    {$IFDEF IP_LAZARUS}
    procedure ParseStyleSheet(Parent: TIpHtmlNode; HRef: String);
    {$ENDIF}
    procedure ParseBodyText(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBlock(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseInline(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseHeader(Parent : TIpHtmlNode; EndToken : TIpHtmlToken; Size : Integer);
    procedure ParseText(const EndTokens: TIpHtmlTokenSet; Parent: TIpHtmlNode);
    procedure ParseFont(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseAddress(Parent: TIpHtmlNode);
    procedure ParseForm(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    function ParseMethod: TIpHtmlFormMethod;
    procedure ParseTableRow(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    function ParseInputType : TIpHtmlInputType;
    procedure ParseFormFields(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure InvalidateRect(R : TRect);
    procedure SetDefaultProps;
    function BuildPath(const Ext: string): string;
    procedure MakeVisible(const R: TRect{$IFDEF IP_LAZARUS}; ShowAtTop: Boolean = True{$ENDIF});
    procedure InvalidateSize;
    procedure AddGifQueue(Graphic: TGraphic; const R: TRect);
    procedure ClearGifQueue;
    procedure StartGifPaint(Target: TCanvas);
    procedure ClearAreaLists;
    function PagePtToScreen(const Pt: TPoint): TPoint;
    procedure NextRealToken;
    procedure SkipTextTokens;
    procedure BuildAreaList;
    procedure ClearAreaList;
    procedure NextNonBlankToken;
    procedure Get(const URL: string);
    procedure Post(const URL: string; FormData: TIpFormDataEntity);
    procedure ClearRectList;
    procedure AddRect(const R: TRect; Node: PIpHtmlElement; Block: TIpHtmlNodeBlock);
    procedure CreateIFrame(Parent: TWinControl; Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
    procedure FinalizeRecs(P: Pointer);
    function LinkVisited(const URL: string): Boolean;
    procedure AddWord(Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
    procedure AddWordEntry(const Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
    function FindElement(const Name: string): TIpHtmlNode;
    procedure Clear; {clear any contents}
    procedure Home;
    function GetPageRect(TargetCanvas: TCanvas; Width, Height : Integer): TRect; // computes the layout for this Canvas
    procedure MouseMove(Pt : TPoint);
    {$IFDEF IP_LAZARUS}
    procedure DeselectAllItems(Item: Pointer);
    {$ENDIF}
    procedure SetSelection(StartPoint, EndPoint: TPoint);
    function HaveSelection: Boolean;
    procedure CopyToClipboard;
    procedure ReportReferences(Node: TIpHtmlNode);
    procedure RequestImageNodes(Node: TIpHtmlNode);
    procedure SelectAll;
    procedure DeselectAll;
    procedure ControlClick(Sender: TIpHtmlNodeControl);
    procedure ControlClick2(Sender: TIpHtmlNodeControl; var cancel: boolean);
    procedure ControlOnEditingDone(Sender: TIpHtmlNodeControl);
    procedure ControlOnChange(Sender: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtmlNodeControl);
    property HotNode: TIpHtmlNode read FHotNode;
    property CurElement: PIpHtmlElement read FCurElement write FCurElement;
    property HotPoint: TPoint read FHotPoint;
    property OnInvalidateRect: TInvalidateEvent read FOnInvalidateRect write FOnInvalidateRect;
    property Target: TCanvas read FTarget;
    property TextColor: TColor read FTextColor write FTextColor;
    property LinkColor: TColor read FLinkColor write FLinkColor;
    property VLinkColor: TColor read FVLinkColor write FVLinkColor;
    property ALinkColor: TColor read FALinkColor write FALinkColor;
    property BgColor: TColor read FBgColor write FBgColor;
    property HasFrames: Boolean read FHasFrames;
    property OnGetImageX: TIpHtmlDataGetImageEvent read FOnGetImageX write FOnGetImageX;
    property OnScroll: TIpHtmlScrollEvent read FOnScroll write FOnScroll;
    property OnInvalidateSize: TNotifyEvent read FOnInvalidateSize write FOnInvalidateSize;
    property OnGet: TGetEvent read FOnGet write FOnGet;
    property OnPost: TPostEvent read FOnPost write FOnPost;
    property OnIFrameCreate: TIFrameCreateEvent read FOnIFrameCreate write FOnIFrameCreate;
    property OnURLCheck: TURLCheckEvent read FOnURLCheck write FOnURLCheck;
    property OnReportURL: TReportURLEvent read FOnReportURL write FOnReportURL;
    property OnControlClick: TControlEvent read FControlClick write FControlClick;
    property OnControlClick2: TControlEvent2 read FControlClick2 write FControlClick2;
    property OnControlEditingDone: TControlEvent read FControlOnEditingDone write FControlOnEditingDone;
    property OnControlChange: TControlEvent read FControlOnChange write FControlOnChange;
    property OnControlCreate: TControlEvent read FControlCreate write FControlCreate;
    property CanPaint: Boolean read FCanPaint;
    property MarginWidth: Integer read FMarginWidth write FMarginWidth default 20;
    property MarginHeight: Integer read FMarginHeight write FMarginHeight default 20;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
    {$IFOPT C+}
    procedure CheckImage(Picture: TPicture);
    {$ENDIF}
    {$IFDEF IP_LAZARUS}
    function GetSelectionBlocks(out StartSelIndex,EndSelIndex: Integer): boolean;
    property CSS: TCSSGlobalProps read FCSS write FCSS;
    {$ENDIF}
    function getControlCount:integer;
    function getControl(i:integer):TIpHtmlNode;
  public
    constructor Create;
    destructor Destroy; override;
    property FlagErrors : Boolean read FFlagErrors write FFlagErrors;
    property FixedTypeface: string read FFixedTypeface write FFixedTypeface;
    property DefaultTypeFace: string read FDefaultTypeFace write FDefaultTypeFace;
    property DefaultFontSize: integer read FDefaultFontSize write FDefaultFontSize;
    property HtmlNode : TIpHtmlNodeHtml read FHtml;
    property CurUrl: string read FCurUrl;
    procedure LoadFromStream(S : TStream);
    procedure Render(TargetCanvas: TCanvas; TargetPageRect : TRect;
      UsePaintBuffer: Boolean; const TopLeft: TPoint);
    property TitleNode : TIpHtmlNodeTITLE read FTitleNode;
    {$IFDEF IP_LAZARUS_DBG}
    procedure DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure DebugAll;
    {$ENDIF}
    property ControlsCount: integer read getControlCount;
    property Controls[i:integer]: TIpHtmlNode read getControl;
    property FrameSet : TIpHtmlNodeFRAMESET read FCurFrameSet;
    property FactBAParag: Real read FFactBAParag write FFactBAParag;
    property MouseLastPoint : TPoint read FMouseLastPoint;
  end;

  {$IFNDEF IP_LAZARUS}
  TIpHtmlFocusRect = class(TCustomControl)
  private
    FAnchor : TIpHtmlNodeA;
  protected
    {HaveFocus : Boolean;}
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF IP_LAZARUS}
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    {$ELSE}
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    property Anchor : TIpHtmlNodeA read FAnchor write FAnchor;
  end;
  {$ENDIF}

  TIpHtmlInternalPanel = class;

  TIpHtmlScrollBar = class
  private
    FKind: TScrollBarKind;
    FIncrement: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FTracking: Boolean;
    FVisible: Boolean;
    procedure SetPosition(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    FControl: TIpHtmlInternalPanel;
    FPageIncrement: TScrollbarInc;
    FCalcRange: Integer;
    FUpdateNeeded: Boolean;
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    procedure DoSetRange(Value: Integer);
    function NeedsScrollBarVisible: Boolean;
    procedure ScrollMessage(var Msg: {$IFDEF IP_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    constructor Create(AControl: TIpHtmlInternalPanel; AKind: TScrollBarKind);
    property Kind: TScrollBarKind read FKind;
    property Increment: TScrollBarInc
                read FIncrement write FIncrement stored False default 8;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer
                read FRange {write SetRange stored IsRangeStored default 0};
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TIpHtmlCustomPanel = class;

  { TIpHtmlInternalPanel }

  TIpHtmlInternalPanel = class(
    {$IFDEF IP_LAZARUS}TCustomControl{$ELSE}TCustomPanel{$ENDIF})
  private
    FHyper : TIpHtml;
    FPageRect : TRect;
    FPageRectValid: boolean;
    FAutoScroll: Boolean;
    FOnHotChange : TNotifyEvent;
    FOnCurElementChange : TNotifyEvent;
    FOnHotClick : TNotifyEvent;
    FOnClick : TNotifyEvent;
    function GetPageRect: TRect;
    procedure SetHtml(const Value: TIpHtml);
    procedure SetPageRect(const Value: TRect);
  protected
    FUpdatingScrollbars : Boolean;
    InPrint: Integer;
    SettingPageRect : Boolean;
    MouseDownX, MouseDownY : Integer;
    HaveSelection,
    MouseIsDown,
    NewSelection : Boolean;
    SelStart, SelEnd : TPoint;
    HintWindow : THintWindow;
    CurHint : string;
    HintX, HintY : Integer;
    HintShownHere : Boolean;
    Printed: Boolean;
    procedure UpdateScrollBars;
    procedure ClearSelection;
    procedure SetSelection;
    procedure ScrollPtInView(P: TPoint);
    procedure ShowHintNow(const NewHint: string);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    {$IFDEF IP_LAZARUS}
    procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    {$ELSE}
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    {$ENDIF}
    {$IFDEF IP_LAZARUS}
    procedure AsyncHotInvoke(data: ptrint);
    {$ENDIF}

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IFDEF IP_LAZARUS}
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$ENDIF}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoHotChange;
    procedure DoCurElementChange;
    procedure DoHotInvoke;
    procedure DoClick;
    procedure Resize; override;
    procedure ScrollInView(R : TRect);
    procedure ScrollInViewRaw(R : TRect);
    function PagePtToScreen(const Pt : TPoint): TPoint;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure HideHint;
    function HtmlPanel: TIpHtmlCustomPanel;
    procedure BeginPrint;
    procedure ResetPrint;
    procedure EndPrint;
  public
    ViewTop, ViewLeft : Integer;
    HScroll,
    VScroll : TIpHtmlScrollBar;
    PrintPageRect : TRect;
    PrintWidth, PrintHeight: Integer;
    PrintTopLeft: TPoint;
    PageCount: Integer;
    procedure InvalidateSize;
    property Hyper : TIpHtml read FHyper write SetHtml;
    property PageRect : TRect read GetPageRect write SetPageRect;
    constructor Create(AOwner: TComponent); override;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property OnHotChange : TNotifyEvent read FOnHotChange write FOnHotChange;
    property OnCurElementChange: TNotifyEvent
                read FOnCurElementChange write FOnCurElementChange;
    property OnHotClick : TNotifyEvent read FOnHotClick write FOnHotClick;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    destructor Destroy; override;
    procedure ScrollRequest(Sender: TIpHtml; const R: TRect{$IFDEF IP_LAZARUS}; ShowAtTop: Boolean = True{$ENDIF});
    function GetPrintPageCount: Integer;
    procedure PrintPages(FromPage, ToPage: Integer);
    procedure PrintPreview;
    function SelectPrinterDlg: boolean;
    procedure EraseBackground(DC: HDC); {$IFDEF IP_LAZARUS} override; {$ENDIF}
  end;

  { TIpAbstractHtmlDataProvider }

  TIpAbstractHtmlDataProvider = class(TIpBaseComponent)
  protected
    function DoGetHtmlStream(const URL: string;
      PostData: TIpFormDataEntity) : TStream; virtual; abstract;
    {$IFDEF IP_LAZARUS}
    function DoGetStream(const URL: string): TStream; virtual; abstract;
    {$ENDIF}
    {-provider assumes ownership of returned TStream and will free it when
      done using it.}
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; virtual; abstract;
    procedure DoLeave(Html: TIpHtml); virtual; abstract;
    procedure DoReference(const URL: string); virtual;  abstract;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); virtual; abstract;
    function CanHandle(const URL: string): Boolean; virtual; abstract;
    // renamed New,Old for IP_LAZARUS to NewURL, OldURL
    function BuildURL(const OldURL, NewURL: string): string; virtual; abstract;
  end;

  TIpHtmlEnumerator = procedure(Document: TIpHtml) of object;

  TIpScrollAction = (hsaHome, hsaEnd, hsaPgUp, hsaPgDn,
    hsaLeft, hsaRight, hsaUp, hsaDown);
  
  TIpHtmlFrame = class
  protected
    FCURURL : string;
    FCurAnchor : string;
    FViewer: TIpHtmlCustomPanel;
    FNoScroll: Boolean;
    FFramePanel : TPanel;
    Pnl : array[0..Pred(IPMAXFRAMES)] of TPanel;
    FMarginWidth, FMarginHeight : Integer;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    FHtml : TIpHtml;
    HyperPanel : TIpHtmlInternalPanel;
    FFrameCount : Integer;
    FFrames : array[0..Pred(IPMAXFRAMES)] of TIpHtmlFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    FParent : TCustomPanel;
    FName : string;
    InOpen: Boolean;
    procedure InvalidateRect(Sender: TIpHtml; const R : TRect);
    procedure FramePanelResize(Sender: TObject);
    procedure AlignPanels;
    procedure InvalidateSize(Sender: TObject);
    procedure Get(Sender: TIpHtml; const URL: string);
    procedure Post(Sender: TIpHtml; const URL: string; FormData: TIpFormDataEntity);
    procedure IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
      Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure ControlClick(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlClick2(Sender: TIpHtml; Node: TIpHtmlNodeControl; var cancel: boolean);
    procedure ControlOnChange(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlOnEditingDone(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure DeselectAll;
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlFrame;
    procedure MakeAnchorVisible(const URL: string);
    procedure Scroll(Action: TIpScrollAction);
    procedure Home;
    function IsExternal(const URL: string): Boolean;
    procedure SetHtml(NewHtml : TIpHtml);
    procedure Stop;
    function getFrame(i: integer): TIpHtmlFrame;
    procedure InternalFreeFrames;
    procedure InternalCreateFrames;
  public
    constructor Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
    destructor Destroy; override;
    procedure OpenURL(const URL: string; Delayed: Boolean);
    property CurUrl: string read FCurUrl;
    property CurAnchor : string read FCurAnchor;
    property Html: TIpHtml read FHtml;
    property FramePanel : TPanel read FFramePanel;
    property Name: string read FName;
    property FrameCount: integer read FFrameCount;
    property Frames[i:integer] : TIpHtmlFrame read getFrame;
    property Viewer: TIpHtmlCustomPanel read FViewer;
  end;

  TIpHtmlCustomScanner = class;
  TIpHtmlNVFrame = class
  protected
    FCURURL : string;
    FCurAnchor : string;
    FScanner: TIpHtmlCustomScanner;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    FHtml : TIpHtml;
    FFrameCount : Integer;
    FFrames : array[0..Pred(IPMAXFRAMES)] of TIpHtmlNVFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    FName : string;
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlNvFrame;
    procedure MakeAnchorVisible(const URL: string);
    procedure Home;
    procedure Stop;
    function getFrame(i: integer): TIpHtmlNVFrame;
  public
    constructor Create(Scanner: TIpHtmlCustomScanner;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
    destructor Destroy; override;
    procedure OpenURL(const URL: string);
    property CurUrl: string read FCurUrl;
    property CurAnchor : string read FCurAnchor;
    property Html: TIpHtml read FHtml;
    property Name: string read FName;
    property FrameCount: integer read FFrameCount;
    property Frames[i:integer] : TIpHtmlNVFrame read getFrame;
    property Scanner: TIpHtmlCustomScanner read FScanner;
  end;

  TIpHtmlControlEvent = procedure(Sender: TIpHtmlCustomPanel;
    Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl)
      of object;

  TIpHtmlControlEvent2 = procedure(Sender: TIpHtmlCustomPanel;
    Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl;
    var cancel: boolean)
      of object;

  TIpHtmlPrintSettings = class(TPersistent)
  private
    FMarginTop: Double;
    FMarginLeft: Double;
    FMarginBottom: Double;
    FMarginRight: Double;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MarginLeft: Double read FMarginLeft write FMarginLeft;
    property MarginTop: Double read FMarginTop write FMarginTop;
    property MarginRight: Double read FMarginRight write FMarginRight;
    property MarginBottom: Double read FMarginBottom write FMarginBottom;
  end;

  { TIpHtmlCustomPanel }

  TIpHtmlCustomPanel = class(TCustomPanel)
  private
    FHotChange : TNotifyEvent;
    FHotClick : TNotifyEvent;
    FControlClick : TIpHtmlControlEvent;
    FControlClick2 : TIpHtmlControlEvent2;
    FControlOnEditingDone : TIpHtmlControlEvent;
    FControlOnChange : TIpHtmlControlEvent;
    FControlCreate : TIpHtmlControlEvent;
    FCurElementChange: TNotifyEvent;
    FDocumentOpen: TNotifyEvent;
    FAllowTextSelect: Boolean;
    FCurElement : PIpHtmlElement;
    FPrintSettings: TIpHtmlPrintSettings;
    FFactBAParag: Real;
    FWantTabs: Boolean;
    procedure SetDataProvider(const AValue: TIpAbstractHtmlDataProvider);
    procedure SetFactBAParag(const Value: Real);
    function FactBAParagNotIs1: Boolean;
    function GetVScrollPos: Integer;
    procedure SetVScrollPos(const Value: Integer);
  protected
    FFlagErrors: Boolean;
    FFixedTypeface: string;
    FDefaultTypeFace: string;
    FDefaultFontSize: integer;
    FHotURL: string;
    FDataProvider: TIpAbstractHtmlDataProvider;
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    VisitedList : TStringList;
    FVLinkColor: TColor;
    FLinkColor: TColor;
    FALinkColor: TColor;
    FTextColor: TColor;
    FBgColor: TColor;
    FShowHints: Boolean;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FMasterFrame : TIpHtmlFrame;
    FHotNode : TIpHtmlNode;
    GetURL : string;
    PostURL : string;
    PostData : TIpFormDataEntity;
    procedure Push(const Target, URL: string);
    function GetTitle: string;
    procedure InternalOpenURL(const Target, HRef: string);
    procedure URLCheck(Sender: TIpHtml; const URL: string; var Visited: Boolean);
    procedure ReportURL(Sender: TIpHtml; const URL: string);
    procedure Paint; override;
    procedure HotChange(Sender: TObject);
    procedure CurElementChange(Sender: TObject);
    procedure HotClick(Sender: TObject);
    procedure ClientClick(Sender: TObject);
    procedure DoHotChange;
    procedure DoHotClick;
    procedure DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMIpHttpGetRequest(var Message: TMessage); message CM_IpHttpGetRequest;
    procedure ControlClick(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlClick2(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl; var pCancel: boolean);
    procedure ControlOnChange(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlOnEditingdone(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlCreate(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    function GetVersion : string;
    function GetCurUrl: string;
    procedure SetVersion(const Value : string);
    procedure SetDefaultTypeFace(const Value: string);
    procedure SetDefaultFontSize(const Value: integer);
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    function GetPrintPageCount: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); {$IFDEF IP_LAZARUS} override; {$ENDIF}

    procedure CopyToClipboard;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure GoBack;
    function canGoBack : boolean;
    procedure GoForward;
    function canGoForward : boolean;
    function HaveSelection: Boolean;
    property MasterFrame : TIpHtmlFrame read FMasterFrame;
    property HotNode : TIpHtmlNode read FHotNode;
    function IsURLHtml(const URL: string): Boolean;
    procedure MakeAnchorVisible(const Name: string);
    {$IF defined(VERSION4) and not defined(IP_LAZARUS)}
    procedure MouseWheelHandler(Var Message: TMessage); Override;
    {$ENDIF}
    procedure OpenURL(const URL: string);
    procedure Scroll(Action: TIpScrollAction);
    procedure SelectAll;
    procedure DeselectAll;
    procedure SetHtml(NewHtml : TIpHtml);
    procedure SetHtmlFromStr(NewHtml : string);
    procedure SetHtmlFromStream(NewHtml : TStream);
    procedure Stop;
    procedure Print(FromPg, ToPg: LongInt);
    procedure PrintPreview;
    function GetContentSize: TSize;

    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property BgColor: TColor read FBgColor write FBgColor default clWhite;
    property ALinkColor: TColor read FALinkColor write FALinkColor default clRed;
    property AllowTextSelect: Boolean read FAllowTextSelect write FAllowTextSelect default True;
    property CurElement: PIpHtmlElement read FCurElement;
    property DataProvider: TIpAbstractHtmlDataProvider read FDataProvider write SetDataProvider;
    property FactBAParag: Real read FFactBAParag write SetFactBAParag stored FactBAParagNotIs1;
    property FlagErrors: Boolean read FFlagErrors write FFlagErrors;
    property FixedTypeface: string read FFixedTypeface write FFixedTypeface;
    property DefaultTypeFace: string read FDefaultTypeFace write SetDefaultTypeFace;
    property DefaultFontSize: integer read FDefaultFontSize write SetDefaultFontSize;
    property HotURL: string read FHotURL;
    property LinkColor: TColor read FLinkColor write FLinkColor default clBlue;
    property MarginHeight: Integer read FMarginHeight write FMarginHeight default 10;
    property MarginWidth: Integer read FMarginWidth write FMarginWidth default 10;
    property PrintSettings: TIpHtmlPrintSettings read FPrintSettings write FPrintSettings;
    property ShowHints: Boolean read FShowHints write FShowHints default True;
    property TextColor: TColor read FTextColor write FTextColor default clBlack;
    property Title: string read GetTitle;
    property VLinkColor: TColor read FVLinkColor write FVLinkColor default clMaroon;

    property OnControlClick: TIpHtmlControlEvent read FControlClick write FControlClick;
    property OnControlClick2: TIpHtmlControlEvent2 read FControlClick2 write FControlClick2;
    property OnControlEditingDone: TIpHtmlControlEvent read FControlOnEditingDone
                                                      write FControlOnEditingDone;
    property OnControlChange: TIpHtmlControlEvent read FControlOnChange write FControlOnChange;
    property OnControlCreate: TIpHtmlControlEvent read FControlCreate write FControlCreate;
    property OnCurElementChange: TNotifyEvent read FCurElementChange write FCurElementChange;
    property OnDocumentOpen: TNotifyEvent read FDocumentOpen write FDocumentOpen;
    property OnHotChange: TNotifyEvent read FHotChange write FHotChange;
    property OnHotClick: TNotifyEvent read FHotClick write FHotClick;
    property CurURL: string read GetCurUrl;
    property WantTabs: Boolean read FWantTabs write FWantTabs default True;
  published
    property Version: string read GetVersion write SetVersion stored False;
  end;

  TIpHtmlPanel = class(TIpHtmlCustomPanel)
  published
    property Align;
    property ALinkColor;
    property AllowTextSelect;
    {$IFDEF VERSION4}
    property Anchors;
    {$ENDIF}
    property BorderWidth;
    property BorderStyle;
    {$IFDEF VERSION4}
    property Constraints;
    {$ENDIF}
    property DataProvider;
    property Enabled;
    property FixedTypeface;
    property DefaultTypeFace;
    property DefaultFontSize;
    property FactBAParag;
    property FlagErrors;
    property LinkColor;
    property MarginHeight;
    property MarginWidth;
    property PopupMenu;
    property PrintSettings;
    property ShowHints;
    property TabOrder;
    property TabStop;
    property TextColor;
    property Visible;
    property VLinkColor;
    property WantTabs;
    {$IF defined(VERSION4) and not defined(IP_LAZARUS)}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    {$IFDEF VERSION4}
    property OnConstrainedResize;
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;
    {$ENDIF}
    property OnControlClick;
    property OnControlClick2;
    property OnControlChange;
    property OnControlEditingDone;
    property OnControlCreate;
    property OnCurElementChange;
    property OnDocumentOpen;
    property OnEnter;
    property OnExit;
    property OnHotChange;
    property OnHotClick;
  end;

  TIpHtmlCustomScanner = class(TComponent)
  private
    FDataProvider: TIpAbstractHtmlDataProvider;
    FFlagErrors: Boolean;
    function GetTitle: string;
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  protected
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    FCurURL : string;
    FMasterFrame : TIpHtmlNVFrame;
    procedure Push(const Target, URL: string);
    procedure InternalOpenURL(const Target, HRef: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    function IsURLHtml(const URL: string): Boolean;
    procedure OpenURL(const URL: string);
    procedure Stop;

    property MasterFrame : TIpHtmlNVFrame read FMasterFrame;
    property DataProvider: TIpAbstractHtmlDataProvider read FDataProvider write FDataProvider;
    property FlagErrors : Boolean read FFlagErrors write FFlagErrors;
    property Title : string read GetTitle;
    property CurUrl: string read FCurUrl;
  published
    property Version : string read GetVersion write SetVersion stored False;
  end;

  TIpHtmlScanner = class(TIpHtmlCustomScanner)
  published
    property DataProvider;
    property FlagErrors;
  end;
  
var
  ScaleFonts : Boolean = False; {true during print preview only}
    {public to let print preview unit access it}
    
function MaxI2(const I1, I2: Integer) : Integer;
function MinI2(const I1, I2: Integer) : Integer;

function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr;

function GetAlignmentForStr(str: string;
         pDefault: TIpHtmlAlign = haDefault) : TIpHtmlAlign;

function AreHtmlMarginsEqual(const Margin1, Margin2: TIpHtmlElemMargin): boolean;

function dbgs(et: TElementType): string; overload;

procedure Register;

implementation

uses
  Printers, IpHtmlPv, PrintersDlgs;

{$R *.res}

{$IFDEF IP_LAZARUS}
{$I ipcss.inc}

{$ENDIF}

var
  FlatSB_GetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    var ScrollInfo: TScrollInfo): BOOL; stdcall;
  FlatSB_GetScrollPos: function(hWnd: HWND; nBar: Integer): Integer; stdcall;
  FlatSB_SetScrollPos: function(hWnd: HWND; nBar, nPos: Integer;
    bRedraw: BOOL): Integer; stdcall;
  FlatSB_SetScrollProp: function(p1: HWND; index: Integer; newValue: Integer;
    p4: Bool): Bool; stdcall;
  FlatSB_SetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;

const
  MaxElements = 1024*1024;
  ShyChar = #1; {character used to represent soft-hyphen in strings}
  NbspChar = #2; {character used to represent no-break space in strings}
  NAnchorChar = #3 ; {character used to represent an Anchor }
  WheelDelta = 8;

const
  WSB_PROP_CYVSCROLL      = $00000001;
  WSB_PROP_CXHSCROLL      = $00000002;
  WSB_PROP_CYHSCROLL      = $00000004;
  WSB_PROP_CXVSCROLL      = $00000008;
  WSB_PROP_CXHTHUMB       = $00000010;
  WSB_PROP_CYVTHUMB       = $00000020;
  WSB_PROP_VBKGCOLOR      = $00000040;
  WSB_PROP_HBKGCOLOR      = $00000080;
  WSB_PROP_VSTYLE         = $00000100;
  WSB_PROP_HSTYLE         = $00000200;
  WSB_PROP_WINSTYLE       = $00000400;
  WSB_PROP_PALETTE        = $00000800;
  WSB_PROP_MASK           = $00000FFF;
  FSB_FLAT_MODE               = 2;
  FSB_ENCARTA_MODE            = 1;
  FSB_REGULAR_MODE            = 0;

var
  ScaleBitmaps : Boolean = False;
  BWPrinter: Boolean;
  Aspect : Double;

{$IFDEF IP_LAZARUS_DBG}
procedure DumpTIpHtmlProps(aProps: TIpHtmlProps);
var
   propA : TIpHtmlPropAFieldsRec;
   propB : TIpHtmlPropBFieldsRec;
begin
     if aProps = nil then
     begin
          writeln('TIpHtmlProps is nil');
          exit;
     end;
     writeln('>>> ', aProps.FOwner.ClassName, ': ', dbgs(@aProps));
     if aProps.PropA <> nil then
     begin
     propA := aProps.PropA.FPropRec;
     writeln('PropA >>>:');
     writeln('BaseFontSize :', propA.BaseFontSize);
     writeln('FontSize :', propA.FontSize);
     //writeln('FontStyle :', propA.FontStyle);
     writeln('FontName :', propA.FontName);
     end;

     if aProps.PropB <> nil then
     begin
     propB := aProps.PropB.FPropRec;
     writeln('PropB >>>:');
     writeln('FontBaseline :', propB.FontBaseline);
     writeln('Alignment :', Ord(propB.Alignment));
     writeln('FontColor :', propB.FontColor);
     writeln('VAlignment :', Ord(propB.VAlignment));
     writeln('LinkColor :', propB.LinkColor);
     writeln('VLinkColor :', propB.VLinkColor);
     writeln('ALinkColor :', propB.ALinkColor);
     writeln('BgColor :', propB.BgColor);
     writeln('NoBreak :', propB.NoBreak);
     end;
end;

procedure DebugBox(Canvas: TCanvas; R: TRect; cl:TColor; dbg:boolean=false);
var
  OldPenColor: TColor;
begin
  OldPenColor := Canvas.Pen.Color;
  Canvas.Pen.Color := cl;
  Canvas.Moveto(r.left+(r.right-r.left) div 2, r.top);
  Canvas.Lineto(r.left+(r.right-r.left) div 2, r.bottom);
  Canvas.MoveTo(r.Left, r.top+(r.bottom-r.top) div 2);
  Canvas.LineTo(r.right, r.top+(r.bottom-r.top) div 2);
  if Dbg then
    DebugLn('DebugBox:R=',dbgs(R));
  Canvas.Pen.Color := OldPenColor;
end;
{$ENDIF}

function CalcBorderColor(AColor: TColor; AStyle: TCSSBorderStyle; ASide: TIpHtmlFrameProp): TColor;
begin
  case AStyle of
    cbsRidge,
    cbsInset:
      if ASide in [hfAbove, hfLhs] then
        Result := ColorAdjustLuma(AColor, -60, False)
      else
        Result := ColorAdjustLuma(AColor, 60, False);
    cbsGroove,
    cbsOutset:
    if ASide in [hfAbove, hfLhs] then
      Result := ColorAdjustLuma(AColor, 60, False)
    else
      Result := ColorAdjustLuma(AColor, -60, False);
  else
    Result := AColor;
  end;
end;

function AreHtmlMarginsEqual(const Margin1, Margin2: TIpHtmlElemMargin): boolean;
begin
  Result:=(Margin1.Style=Margin2.Style)
       and (Margin1.Size=Margin2.Size);
end;

function dbgs(et: TElementType): string;
begin
  writestr(Result,et);
end;

procedure Register;
begin
  RegisterComponents('IPro', [TIpHtmlPanel]);
end;

{$IFNDEF VERSION3ONLY}
type
  THtmlRadioButton = class(TRadioButton)
  protected
    FChecked: Boolean;
    procedure SetChecked(Value: Boolean); override;
    function GetChecked: Boolean; override;
    procedure CreateWnd; override;
end;

procedure THtmlRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  {$IFNDEF IP_LAZARUS}
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
  {$ENDIF}
end;

function THtmlRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure THtmlRadioButton.SetChecked(Value: Boolean);
{$IFDEF IP_LAZARUS}
begin
  inherited SetChecked(Value);
end;
{$ELSE IP_LAZARUS}

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do begin
          Sibling := Controls[I];
          if (Sibling <> Self)
          and (Sibling is THtmlRadioButton)
          and (Sibling.Tag = Self.Tag) then
            with THtmlRadioButton(Sibling) do
              SetChecked(False);
        end;
  end;

begin
  if FChecked <> Value then begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
    if Value then begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then
        Click;
    end;
  end;
end;
{$ENDIF IP_LAZARUS}

{$ENDIF}

function GetAlignmentForStr(str: string;
         pDefault: TIpHtmlAlign = haDefault) : TIpHtmlAlign;
var
  S : string;
begin
  S := UpperCase(str);
  if length(S) = 0 then
  begin
       Result := pDefault;
       exit;
  end;
  case S[1] of
    'C','M': if S = 'CHAR' then Result := haChar
             else if (S = 'CENTER') or (S = 'MIDDLE') then
               Result := haCenter;
    'J': if S = 'JUSTIFY' then Result := haJustify;
    'L': if (S = 'LEFT') then Result := haLeft;
    'R': if S = 'RIGHT' then Result := haRight;
    else Result := pDefault;
  end;
end;

procedure GetRelativeAspect(PrinterDC : hDC);
var
  ScreenDC : hDC;
begin
  ScreenDC := GetDC(0);
  try
    Aspect :=
      {$IFDEF IP_LAZARUS}
      Printer.XDPI
      {$ELSE}
      GetDeviceCaps(PrinterDC, LOGPIXELSX)
      {$ENDIF}
      / GetDeviceCaps(ScreenDC, LOGPIXELSX);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

{$IFDEF IP_LAZARUS}
constructor TIpHtmlPoolManager.Create(TheItemSize, MaxItems : DWord);
begin
  inherited Create(TheItemSize);
  ClearOnCreate:=true;
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  Result:=NewItem;
end;

{$ELSE IP_LAZARUS}

constructor TIpHtmlPoolManager.Create(ItemSize, MaxItems : DWord);
begin
  InitializeCriticalSection(Critical);
  EnterCriticalSection(Critical);
  try
    InternalSize := ItemSize;
    while 4096 mod InternalSize <> 0 do
      Inc(InternalSize);
    Root := VirtualAlloc(nil, InternalSize * MaxItems,
      MEM_RESERVE, PAGE_NOACCESS);
    NextPage := Root;
    Next := Root;
  finally
    LeaveCriticalSection(Critical);
  end;
  {Top := Pointer(DWord(Root) + InternalSize * MaxItems);}
end;

destructor TIpHtmlPoolManager.Destroy;
begin
  EnterCriticalSection(Critical);
  try
    if Root <> nil then
      VirtualFree(Root, 0, MEM_RELEASE);
    inherited Destroy;
  finally
    LeaveCriticalSection(Critical);
  end;
  DeleteCriticalSection(Critical);
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  EnterCriticalSection(Critical);
  if Next = NextPage then
    Grow;
  Result := Next;
  Inc(DWord(Next), InternalSize);
  LeaveCriticalSection(Critical);
end;

procedure TIpHtmlPoolManager.Grow;
var
  P: Pointer;
begin
  P := VirtualAlloc(NextPage, 4096, MEM_COMMIT, PAGE_READWRITE);
  if P = nil then
    raise Exception.Create('Out of memory');
  Inc(DWord(NextPage),4096);
end;

procedure TIpHtmlPoolManager.EnumerateItems(Method: TIpEnumItemsMethod);
var
  P : Pointer;
begin
  P := Root;
  while DWord(P) < DWord(Next) do begin
    Method(P);
    Inc(DWord(P), InternalSize);
  end;
end;
{$ENDIF IP_LAZARUS}


{$IFNDEF IP_LAZARUS}
// workaround for fpc bug: local string constants
function ParseConstant(const S: string): AnsiChar;
{$ENDIF}
Const
  CodeCount = 126; //JMN
  {Sorted by Size where size is Length(Name).
  Make sure you respect this when adding new items}
  Codes: array[0..Pred(CodeCount)] of record
    Size: Integer;
    Name: String;
    Value: String;
    ValueUtf8: String; //UTF8 DiBo33
  end = (
    (Size: 2; Name: 'gt';   Value: '>'; ValueUtf8: #$3E),
    (Size: 2; Name: 'lt';   Value: '<'; ValueUtf8: #$3C),
    (Size: 3; Name: 'amp';  Value: '&'; ValueUtf8: #$26),
    (Size: 3; Name: 'deg';  Value: #176; ValueUtf8: #$C2#$B0),
    (Size: 3; Name: 'ETH';  Value: #208; ValueUtf8: #$C3#$90),
    (Size: 3; Name: 'eth';  Value: #240; ValueUtf8: #$C3#$B0),
    (Size: 3; Name: 'not';  Value: #172; ValueUtf8: #$C2#$AC),
    (Size: 3; Name: 'reg';  Value: #174; ValueUtf8: #$C2#$AE),
    (Size: 3; Name: 'shy';  Value: ShyChar; ValueUtf8: ShyChar),
    (Size: 3; Name: 'uml';  Value: #168; ValueUtf8: #$C2#$A8),
    (Size: 3; Name: 'yen';  Value: #165; ValueUtf8: #$C2#$A5),
    (Size: 4; Name: 'Auml'; Value: #196; ValueUtf8: #$C3#$84),
    (Size: 4; Name: 'auml'; Value: #228; ValueUtf8: #$C3#$A4),
    (Size: 4; Name: 'bull'; Value: #149; ValueUtf8: #$E2#$80#$A2),
    (Size: 4; Name: 'cent'; Value: #162; ValueUtf8: #$C2#$A2),
    (Size: 4; Name: 'circ'; Value: '^';  ValueUtf8: #$5E),
    (Size: 4; Name: 'copy'; Value: #169; ValueUtf8: #$C2#$A9),
    (Size: 4; Name: 'Euml'; Value: #203; ValueUtf8: #$C3#$8B),
    (Size: 4; Name: 'euml'; Value: #235; ValueUtf8: #$C3#$AB),
    (Size: 4; Name: 'euro'; Value: #128; ValueUtf8: #$E2#$82#$AC),
    (Size: 4; Name: 'fnof'; Value: #131; ValueUtf8: #$C6#$92),
    (Size: 4; Name: 'Iuml'; Value: #207; ValueUtf8: #$C3#$8F),
    (Size: 4; Name: 'iuml'; Value: #239; ValueUtf8: #$C3#$AF),
    (Size: 4; Name: 'macr'; Value: #175; ValueUtf8: #$C2#$AF),
    (Size: 4; Name: 'nbsp'; Value: NbspChar; ValueUtf8: NbspChar),
    (Size: 4; Name: 'ordf'; Value: #170; ValueUtf8: #$C2#$AA),
    (Size: 4; Name: 'ordm'; Value: #186; ValueUtf8: #$C2#$BA),
    (Size: 4; Name: 'Ouml'; Value: #214; ValueUtf8: #$C3#$96),
    (Size: 4; Name: 'ouml'; Value: #246; ValueUtf8: #$C3#$B6),
    (Size: 4; Name: 'para'; Value: #182; ValueUtf8: #$C2#$B6),
    (Size: 4; Name: 'quot'; Value: '"';  ValueUtf8: #$22),
    (Size: 4; Name: 'sect'; Value: #167; ValueUtf8: #$C2#$A7),
    (Size: 4; Name: 'sup1'; Value: #185; ValueUtf8: #$C2#$B9),
    (Size: 4; Name: 'sup2'; Value: #178; ValueUtf8: #$C2#$B2),
    (Size: 4; Name: 'sup3'; Value: #179; ValueUtf8: #$C2#$B3),
    (Size: 4; Name: 'Uuml'; Value: #220; ValueUtf8: #$C3#$9C),
    (Size: 4; Name: 'uuml'; Value: #252; ValueUtf8: #$C3#$BC),
    (Size: 4; Name: 'Yuml'; Value: #159; ValueUtf8: #$C5#$B8),
    (Size: 4; Name: 'yuml'; Value: #255; ValueUtf8: #$C3#$BF),
    (Size: 5; Name: 'Acirc'; Value: #194; ValueUtf8: #$C3#$82),
    (Size: 5; Name: 'acirc'; Value: #226; ValueUtf8: #$C3#$A2),
    (Size: 5; Name: 'acute'; Value: #180; ValueUtf8: #$C2#$B4),
    (Size: 5; Name: 'AElig'; Value: #198; ValueUtf8: #$C3#$86),
    (Size: 5; Name: 'aelig'; Value: #230; ValueUtf8: #$C3#$A6),
    (Size: 5; Name: 'Aring'; Value: #197; ValueUtf8: #$C3#$85),
    (Size: 5; Name: 'aring'; Value: #229; ValueUtf8: #$C3#$A5),
    (Size: 5; Name: 'cedil'; Value: #184; ValueUtf8: #$C2#$B8),
    (Size: 5; Name: 'Ecirc'; Value: #202; ValueUtf8: #$C3#$8A),
    (Size: 5; Name: 'ecirc'; Value: #234; ValueUtf8: #$C3#$AA),
    (Size: 5; Name: 'frasl'; Value: '/';  ValueUtf8: #$2F),
    (Size: 5; Name: 'Icirc'; Value: #206; ValueUtf8: #$C3#$8E),
    (Size: 5; Name: 'icirc'; Value: #238; ValueUtf8: #$C3#$AE),
    (Size: 5; Name: 'iexcl'; Value: #161; ValueUtf8: #$C2#$A1),
    (Size: 5; Name: 'laquo'; Value: #171; ValueUtf8: #$C2#$AB),
    (Size: 5; Name: 'ldquo'; Value: #147; ValueUtf8: #$E2#$80#$9C),
    (Size: 5; Name: 'lsquo'; Value: #145; ValueUtf8: #$E2#$80#$98),
    (Size: 5; Name: 'mdash'; Value: #151; ValueUtf8: #$E2#$80#$94),
    (Size: 5; Name: 'micro'; Value: #181; ValueUtf8: #$C2#$B5),
    (Size: 5; Name: 'minus'; Value: '-';  ValueUtf8: #$2D),
    (Size: 5; Name: 'ndash'; Value: #150; ValueUtf8: #$E2#$80#$93),
    (Size: 5; Name: 'Ocirc'; Value: #212; ValueUtf8: #$C3#$94),
    (Size: 5; Name: 'ocirc'; Value: #244; ValueUtf8: #$C3#$B4),
    (Size: 5; Name: 'OElig'; Value: #140; ValueUtf8: #$C5#$92),
    (Size: 5; Name: 'oelig'; Value: #156; ValueUtf8: #$C5#$93),
    (Size: 5; Name: 'pound'; Value: #163; ValueUtf8: #$C2#$A3),
    (Size: 5; Name: 'raquo'; Value: #187; ValueUtf8: #$C2#$BB),
    (Size: 5; Name: 'rdquo'; Value: #148; ValueUtf8: #$E2#$80#$9D),
    (Size: 5; Name: 'rsquo'; Value: #146; ValueUtf8: #$E2#$80#$99),
    (Size: 5; Name: 'szlig'; Value: #223; ValueUtf8: #$C3#$9F),
    (Size: 5; Name: 'THORN'; Value: #222; ValueUtf8: #$C3#$9E),
    (Size: 5; Name: 'thorn'; Value: #254; ValueUtf8: #$C3#$BE),
    (Size: 5; Name: 'tilde'; Value: '~';  ValueUtf8: #$7E),
    (Size: 5; Name: 'times'; Value: #215; ValueUtf8: #$C3#$97),
    (Size: 5; Name: 'trade'; Value: #153; ValueUtf8: #$E2#$84#$A2),
    (Size: 5; Name: 'Ucirc'; Value: #219; ValueUtf8: #$C3#$9B),
    (Size: 5; Name: 'ucirc'; Value: #251; ValueUtf8: #$C3#$BB),
    (Size: 6; Name: 'Aacute'; Value: #193; ValueUtf8: #$C3#$81),
    (Size: 6; Name: 'aacute'; Value: #225; ValueUtf8: #$C3#$A1),
    (Size: 6; Name: 'Agrave'; Value: #192; ValueUtf8: #$C3#$80),
    (Size: 6; Name: 'agrave'; Value: #224; ValueUtf8: #$C3#$A0),
    (Size: 6; Name: 'Atilde'; Value: #195; ValueUtf8: #$C3#$83),
    (Size: 6; Name: 'atilde'; Value: #227; ValueUtf8: #$C3#$A3),
    (Size: 6; Name: 'brvbar'; Value: #166; ValueUtf8: #$C2#$A6),
    (Size: 6; Name: 'Ccedil'; Value: #199; ValueUtf8: #$C3#$87),
    (Size: 6; Name: 'ccedil'; Value: #231; ValueUtf8: #$C3#$A7),
    (Size: 6; Name: 'curren'; Value: #164; ValueUtf8: #$C2#$A4),
    (Size: 6; Name: 'dagger'; Value: #134; ValueUtf8: #$E2#$80#$A0),
    (Size: 6; Name: 'Dagger'; Value: #135; ValueUtf8: #$E2#$80#$A1),
    (Size: 6; Name: 'divide'; Value: #247; ValueUtf8: #$C3#$B7),
    (Size: 6; Name: 'Eacute'; Value: #201; ValueUtf8: #$C3#$89),
    (Size: 6; Name: 'eacute'; Value: #233; ValueUtf8: #$C3#$A9),
    (Size: 6; Name: 'Egrave'; Value: #200; ValueUtf8: #$C3#$88),
    (Size: 6; Name: 'egrave'; Value: #232; ValueUtf8: #$C3#$A8),
    (Size: 6; Name: 'frac12'; Value: #189; ValueUtf8: #$C2#$BD),
    (Size: 6; Name: 'frac14'; Value: #188; ValueUtf8: #$C2#$BC),
    (Size: 6; Name: 'frac34'; Value: #190; ValueUtf8: #$C2#$BE),
    (Size: 6; Name: 'hellip'; Value: #133; ValueUtf8: #$E2#$80#$A6),
    (Size: 6; Name: 'Iacute'; Value: #205; ValueUtf8: #$C3#$8D),
    (Size: 6; Name: 'iacute'; Value: #237; ValueUtf8: #$C3#$AD),
    (Size: 6; Name: 'Igrave'; Value: #204; ValueUtf8: #$C3#$8C),
    (Size: 6; Name: 'igrave'; Value: #236; ValueUtf8: #$C3#$AC),
    (Size: 6; Name: 'iquest'; Value: #191; ValueUtf8: #$C2#$BF),
    (Size: 6; Name: 'lsaquo'; Value: #139; ValueUtf8: #$E2#$80#$B9),
    (Size: 6; Name: 'middot'; Value: #183; ValueUtf8: #$C2#$B7),
    (Size: 6; Name: 'Ntilde'; Value: #209; ValueUtf8: #$C3#$91),
    (Size: 6; Name: 'ntilde'; Value: #241; ValueUtf8: #$C3#$B1),
    (Size: 6; Name: 'Oacute'; Value: #211; ValueUtf8: #$C3#$93),
    (Size: 6; Name: 'oacute'; Value: #243; ValueUtf8: #$C3#$B3),
    (Size: 6; Name: 'Ograve'; Value: #210; ValueUtf8: #$C3#$92),
    (Size: 6; Name: 'ograve'; Value: #242; ValueUtf8: #$C3#$B2),
    (Size: 6; Name: 'Oslash'; Value: #216; ValueUtf8: #$C3#$98),
    (Size: 6; Name: 'oslash'; Value: #248; ValueUtf8: #$C3#$B8),
    (Size: 6; Name: 'Otilde'; Value: #213; ValueUtf8: #$C3#$95),
    (Size: 6; Name: 'otilde'; Value: #245; ValueUtf8: #$C3#$B5),
    (Size: 6; Name: 'permil'; Value: #137; ValueUtf8: #$E2#$80#$B0),
    (Size: 6; Name: 'plusmn'; Value: #177; ValueUtf8: #$C2#$B1),
    (Size: 6; Name: 'rsaquo'; Value: #155; ValueUtf8: #$E2#$80#$BA),
    (Size: 6; Name: 'Scaron'; Value: #138; ValueUtf8: #$C5#$A0),
    (Size: 6; Name: 'scaron'; Value: #154; ValueUtf8: #$C5#$A1),
    (Size: 6; Name: 'Uacute'; Value: #218; ValueUtf8: #$C3#$9A),
    (Size: 6; Name: 'uacute'; Value: #250; ValueUtf8: #$C3#$BA),
    (Size: 6; Name: 'Ugrave'; Value: #217; ValueUtf8: #$C3#$99),
    (Size: 6; Name: 'ugrave'; Value: #249; ValueUtf8: #$C3#$B9),
    (Size: 6; Name: 'Yacute'; Value: #221; ValueUtf8: #$C3#$9D),
    (Size: 6; Name: 'yacute'; Value: #253; ValueUtf8: #$C3#$BD),
    (Size: 6; Name: 'xxxxxx'; Value: NAnchorChar; ValueUtf8: NAnchorChar) //JMN
    );
{$IFDEF IP_LAZARUS}
function ParseConstant(const S: string; onUtf8: boolean=false): string;
{$ENDIF}
var
  Error: Integer;
  Index1: Integer;
  Index2: Integer;
  Size1: Integer;
  Found: Boolean;

begin {'Complete boolean eval' must be off}
  Result := ' ';
  Size1 := Length(S);
  if Size1 = 0 then Exit;
  if (S[1] in ['$', '0'..'9']) then
  begin
    Val(S, Index1, Error);
    if (Error = 0) then
    begin
      if not OnUTF8 and (Index1 >= 32) and (Index1 <= 255) then
        Result := Chr(Index1)
      else
        Result := UnicodeToUTF8(Index1);
    end;
  end else
  begin
    Index1 := 0;
    repeat
      if Size1 = Codes[Index1].Size then
      begin
        Found := True;
        Index2 := 1;
        while Index2 <= Size1 do
        begin
          if S[Index2] <> Codes[Index1].Name[Index2] then
          begin
            Found := False;
            Break;
          end;
          Inc(Index2);
        end;
        if Found then
        begin
          if onUtf8 then Result := Codes[Index1].ValueUTF8
          else Result := Codes[Index1].Value;
          Break;
        end;
      end;
      Inc(Index1);
    until (Index1 >= CodeCount) or (Codes[Index1].Size > Size1);
  end;
end;

procedure ExpandEscapes(var S: string);
{- returns the string with & escapes expanded}
var
  i, j : Integer;
  Co : string;
  Ch : AnsiChar;
{$IFDEF IP_LAZARUS}
  St : string;
{$ENDIF}
begin
  i := length(S);
  while i > 0 do begin
    if S[i] = '&' then begin
      j := i;
      while (j < length(S)) and not (S[j] in [';',' ']) do
        Inc(j);
      Co := copy(S, i + 1, j - i - 1);
      if Co <> '' then begin
        if Co[1] = '#' then begin
          Delete(Co, 1, 1);
          if UpCase(Co[1]) = 'X' then begin
            Delete(Co, 1, 1);
            Insert('$', Co, 1);
          end;
        end;
        Delete(S, i, j - i + 1);
      {$IFDEF IP_LAZARUS}
        if SystemCharSetIsUTF8 then begin
          St := ParseConstant(Co, true);
          Insert(St, S, i)
        end else begin
          Ch := ParseConstant(Co)[1];
          Insert(Ch, S, i);
        end;
      {$ELSE}
        Ch := ParseConstant(Co)[1];
        Insert(Ch, S, i);
      {$ENDIF}
      end;
    end;
    Dec(i);
  end;
end;

function EscapeToAnsi(const S: string): string;
var
  P : Integer;
begin
  Result := S;
  P := CharPos('&', S);
  if P <> 0 then
    ExpandEscapes(Result);
end;

function NoBreakToSpace(const S: string): string;
var
  P : Integer;
begin
  Result := S;
  for P := length(Result) downto 1 do
    if Result[P] = NbspChar then
      Result[P] := ' ';
end;

procedure SetRawWordValue(Entry: PIpHtmlElement; const Value: string);
var
  L : Integer;
begin
  Entry.AnsiWord := EscapeToAnsi(Value);
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      Inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
end;

procedure SetWordRect(Element: PIpHtmlElement; const Value: TRect);
begin
  Element.WordRect2 := Value;
  if Element.ElementType = etObject then begin
    if (Value.Left < Value.Right)
    and (Value.Bottom > Value.Top)
    and (Value.Left >= 0) and (Value.Top >= 0) then
      TIpHtmlNodeAlignInline(Element.Owner).SetRect(Value);
  end;
end;

type
  TFriendPanel = class(TCustomPanel) end;

const
  LF = #10;
  CR = #13;
  {StdIndent = 16;}
  NullRect : TRect = (Left:0; Top:0; Right:0; Bottom:0);

{$IFNDEF IP_LAZARUS}
//{$R IpHtml.res} // JMN
{$EndIf}

function StdIndent: Integer;
begin
  if ScaleBitmaps and (Aspect > 0) then
    Result := round(16 * Aspect)
  else
    Result := 16;
end;

function SizeRec(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

function MaxI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 > I1 then
    Result := I2;
end;

function MaxI3(const I1, I2, I3: Integer) : Integer;
begin
  if I2 > I1 then
    if I3 > I2 then
      Result := I3
    else
      Result := I2
  else
    if I3 > I1 then
      Result := I3
    else
      Result := I1;
end;

function MinI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 < I1 then
    Result := I2;
end;

function SameDimensions(const R1, R2 : TRect): Boolean;
begin
  Result :=
    (
    ((R1.Bottom - R1.Top) = (R2.Bottom - R2.Top))
    or (R1.Top = R2.Top))
     and
    ((R1.Right - R1.Left) = (R2.Right - R2.Left));
end;

function FirstString(const S: string): string;
{- returns first string if a list - otherwise the string itself}
var
  P : Integer;
begin
  P := CharPos(',', S);
  if P = 0 then
    Result := S
  else
    Result := copy(S, 1, P - 1);
end;

{ TIpHtmlInteger }

constructor TIpHtmlInteger.Create(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TIpHtmlInteger.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlInteger.GetValue: Integer;
begin
  if ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlInteger.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlPixels }

procedure TIpHtmlPixels.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlPixels.GetValue: Integer;
begin
  if (PixelsType = hpAbsolute) and ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlPixels.SetPixelsType(const Value: TIpHtmlPixelsType);
begin
  if Value <> FPixelsType then begin
    FPixelsType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlPixels.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlRelSize }

procedure TIpHtmlRelSize.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

procedure TIpHtmlRelSize.SetSizeType(const Value: TIpHtmlRelSizeType);
begin
  if Value <> FSizeType then begin
    FSizeType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlRelSize.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlLength }

procedure TIpHtmlLength.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlLength.GetLengthValue: Integer;
begin
  if (LengthType = hlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;

procedure TIpHtmlLength.SetLengthType(const Value: TIpHtmlLengthType);
begin
  if Value <> FLengthType then begin
    FLengthType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlLength.SetLengthValue(const Value: Integer);
begin
  if Value <> FLengthValue then begin
    FLengthValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlMultiLength }

function TIpHtmlMultiLength.GetLengthValue: Integer;
begin
  if (LengthType = hmlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;

{ TIpHtmlMultiLengthList }

procedure TIpHtmlMultiLengthList.AddEntry(Value: TIpHtmlMultiLength);
begin
  List.Add(Value);
end;

procedure TIpHtmlMultiLengthList.Clear;
begin
  while List.Count > 0 do begin
    TIpHtmlMultiLength(List[0]).Free;
    List.Delete(0);
  end;
end;

constructor TIpHtmlMultiLengthList.Create;
begin
  List := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
end;

destructor TIpHtmlMultiLengthList.Destroy;
begin
  inherited;
  Clear;
  List.Free;
end;

function TIpHtmlMultiLengthList.GetEntries: Integer;
begin
  Result := List.Count;
end;

function TIpHtmlMultiLengthList.GetValues(
  Index: Integer): TIpHtmlMultiLength;
begin
  Result := TIpHtmlMultiLength(List[Index]);
end;

{ TIpHtmlNode }

function TIpHtmlNode.GetHint: string;
begin
  Result := '';
end;

constructor TIpHtmlNode.Create(ParentNode : TIpHtmlNode);
begin
  if assigned(ParentNode) then
    if ParentNode is TIpHtmlNodeMulti then
      TIpHtmlNodeMulti(ParentNode).FChildren.Add(Self)
    else
      raise EIpHtmlException.Create(SHtmlNotContainer);
  FParentNode := ParentNode;
  if ParentNode <> nil then
    FOwner := ParentNode.Owner;
end;

destructor TIpHtmlNode.Destroy;
begin
  if ((Owner = nil) or not Owner.Destroying)
  and (FParentNode <> nil) then
    TIpHtmlNodeMulti(FParentNode).FChildren.Remove(Self);
end;

function TIpHtmlNode.PageRectToScreen(const Rect: TRect;
  var ScreenRect: TRect): Boolean;
{ -convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    Exit;
  end;
  if not IntersectRect(Tmp, Rect, Owner.PageViewRect) then begin
    Result := False;
    Exit;
  end;
  ScreenRect := Rect;
  with Owner.PageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with Owner.ClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, Owner.ClientRect) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure TIpHtmlNode.ScreenLine(StartPoint, EndPoint : TPoint;const Width : Integer;
  const Color : TColor);
var
  SaveWidth : Integer;
  aPen: TPen;
  aCanvas: TCanvas;
begin
  StartPoint := PagePtToScreen(StartPoint);
  EndPoint := PagePtToScreen(EndPoint);
  aCanvas := Owner.Target;
  aPen:= aCanvas.Pen;
  SaveWidth := aPen.Width;
  aPen.Width := Width;
  aPen.Color := Color;
  aCanvas.MoveTo(StartPoint.x, StartPoint.y);
  aCanvas.LineTo(EndPoint.x, EndPoint.y);
  aPen.Width := SaveWidth;
end;

procedure TIpHtmlNode.ScreenRect(R : TRect; const Color : TColor);
begin
  if PageRectToScreen(R, R) then begin
    with Owner.Target do begin
      {$IFDEF IP_LAZARUS}
      Brush.Style := bsSolid;
      {$ENDIF}
      Brush.Color := Color;
      FrameRect(R);
    end;
  end;
end;
{$IFDEF IP_LAZARUS}
procedure TIpHtmlNode.ScreenFrame(R : TRect; Raised: boolean);
var
  SaveWidth: Integer;
  procedure DoLine(X1,Y1,X2,Y2: Integer; Clr: TColor);
  begin
    with Owner.Target do begin
      Pen.Color := Clr;
      Line(X1,Y1,X2,Y2);
      //MoveTo(X1, Y1);
      //LineTo(X2, Y2);
    end;
  end;
begin
  if PageRectToScreen(R, R) then
  with Owner.Target do begin
    Brush.Style := bsSolid;
    SaveWidth := Pen.Width;
    Pen.Width := 1;
    if Raised then begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(220,220,220)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1, RGB(64,64,64)); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(192,192,192)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(128,128,128)); // Right
    end else begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(64,64,64)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1,RGB(220,220,220) ); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(128,128,128)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(192,192,192)); // Right
    end;
    Pen.Width := SaveWidth;
  end;
end;
{$ENDIF}
procedure TIpHtmlNode.ScreenPolygon(Points : array of TPoint; const Color : TColor);
var
  Pt : TPoint;
  i : Integer;
  SaveColor : TColor;
begin
  for i := 0 to High(Points) do begin
    Pt := PagePtToScreen(Points[i]);
    Points[i] := Pt;
  end;
  with Owner.Target do begin
    Pen.Color := Color;
    SaveColor := Brush.Color;
    Brush.Color := Color;
    Polygon(Points);
    Brush.Color := SaveColor;
  end;
end;

function TIpHtmlNode.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with Owner.PageViewRect do begin
    Dec(Result.x, Left);
    Dec(Result.y, Top);
  end;
  with Owner.ClientRect do begin
    Inc(Result.x, Left);
    Inc(Result.y, Top);
  end;
end;

procedure TIpHtmlNode.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.ReportMapRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.InvalidateSize;
begin
  if FParentNode = nil then
    Owner.InvalidateSize
  else
    FParentNode.InvalidateSize;
end;

procedure TIpHtmlNode.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
begin
  EnumProc(Self, UserData);
end;

procedure TIpHtmlNode.SubmitRequest;
begin
  if FParentNode <> nil then
    FParentNode.SubmitRequest;
end;

procedure TIpHtmlNode.ResetRequest;
begin
  if FParentNode <> nil then
    FParentNode.ResetRequest;
end;

procedure TIpHtmlNode.ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod);
begin
  if FParentNode <> nil then
    FParentNode.ReportCurDrawRects(Owner, M);
end;

procedure TIpHtmlNode.AppendSelection(var S: string);
begin
end;

procedure TIpHtmlNode.CreateControl(Parent: TWinControl);
begin
end;

procedure TIpHtmlNode.Enqueue;
begin

end;

procedure TIpHtmlNode.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

function TIpHtmlNode.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNode.HideUnmarkedControl;
begin
end;

procedure TIpHtmlNode.ImageChange(NewPicture: TPicture);
begin
end;

procedure TIpHtmlNode.Invalidate;
begin
end;

procedure TIpHtmlNode.MakeVisible;
begin
end;

procedure TIpHtmlNode.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNode.UnmarkControl;
begin
end;

{!!.10 attribute support code - new}

function GetPropertyValue(PI: PPropInfo; const AObject: TObject): string;

  function GetPropType : PTypeInfo;
  begin
    Result := PI.PropType{$IFDEF VERSION3}^{$ENDIF};
  end;

  function GetIntegerProperty : string;
  begin
    Result := IntToStr(GetOrdProp(AObject, PI));
  end;

  function GetCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetEnumProperty : string;
  begin
    Result := GetEnumName(GetPropType, GetOrdProp(AObject, PI));
  end;

  function GetFloatProperty : string;
  const
    Precisions : array[TFloatType] of Integer = (7, 15, 18, 18, 19);
  begin
    Result := FloatToStrF(GetFloatProp(AObject, PI), ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
  end;

  function GetLStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  function GetWCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetVariantProperty : string;
  begin
    {$IFDEF FPC}
    Result := AnsiString(GetVariantProp(AObject, PI));
    {$ELSE}
    Result := GetVariantProp(AObject, PI);
    {$ENDIF}
  end;

  function GetStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  function GetSetProperty : string;
  var
    TypeInfo : PTypeInfo;
    W        : Cardinal;
    I        : Integer;
  begin
    Result := '[';
    W := GetOrdProp(AObject, PI);
    TypeInfo := GetTypeData(GetPropType)^.CompType{$IFNDEF IP_LAZARUS}^{$ENDIF};
    for I := 0 to Pred(sizeof(Cardinal) * 8) do
      if I in TCardinalSet(W) then begin
        if Length(Result) <> 1 then
          Result := Result + ',';
        Result := Result + GetEnumName(TypeInfo, I);
      end;
    Result := Result + ']';
  end;


begin
  Result := '??';
  case PI.PropType^.Kind of
    tkInteger      : Result := GetIntegerProperty;
    tkChar         : Result := GetCharProperty;
    tkEnumeration  : Result := GetEnumProperty;
    tkFloat        : Result := GetFloatProperty;
    tkLString      : Result := GetLStringProperty;
    tkWChar        : Result := GetWCharProperty;
    tkVariant      : Result := GetVariantProperty;
    tkString       : Result := GetStringProperty;
    tkSet          : Result := GetSetProperty;
    else
      Result := 'unsupported';
  end;
end;

procedure SetPropertyValueLow(PI: PPropInfo;
  const AObject: TObject; const NewValue: string);

  function GetPropType : PTypeInfo;
  begin
    Result := PI.PropType{$IFDEF VERSION3}^{$ENDIF};
  end;

  procedure SetIntegerProperty;
  begin
    SetOrdProp(AObject, PI, StrToInt(NewValue));
  end;

  procedure SetCharProperty;
  begin
    SetOrdProp(AObject, PI, ord(NewValue[1]));
  end;

  procedure SetEnumProperty;
  begin
    {$IFDEF VERSION5}
    SetEnumProp(AObject, PI, NewValue);
    {$ENDIF}
  end;

  procedure SetFloatProperty;
  begin
    SetFloatProp(AObject, PI, StrToFloat(NewValue));
  end;

  procedure SetStringProperty;
  begin
    SetStrProp(AObject, PI, NewValue);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  procedure SetSetProperty;
  begin
    {$IFDEF VERSION5}
    SetSetProp(AObject, PI, NewValue);
    {$ENDIF}
  end;

begin
  if not assigned(PI.SetProc) then
    raise Exception.Create('Property is read-only');
  case PI.PropType^.Kind of
    tkInteger      : SetIntegerProperty;
    tkChar         : SetCharProperty;
    tkEnumeration  : SetEnumProperty;
    tkFloat        : SetFloatProperty;
    tkLString      : SetStringProperty;
    tkString       : SetStringProperty;
    tkSet          : SetSetProperty;
    else
      raise Exception.Create('Unsupported attribute type');
  end;
end;

function GetPropertyList(C: TObject; IncludeValues, IncludeBlanks: Boolean): TStringList;
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  S: string;
  SubList: TStringList;
  O: TObject;
begin
  Result := TStringList.Create;
  try
    if C.ClassInfo <> nil then begin
      LCount := GetPropList(C.ClassInfo, tkProperties, nil);
      LSize := LCount * SizeOf(Pointer);
      if LSize > 0 then begin
        GetMem(PList, LSize);
        try
          GetPropList(C.ClassInfo, tkProperties, PList);
          for I := 0 to LCount-1 do begin
            if PList^[I].PropType^.Kind = tkClass then begin
              {SubList := TStringList.Create;}
              SubList := nil;
              try
                O := TObject(GetOrdProp(C, PList^[I]));
                SubList := GetPropertyList(O, IncludeValues, IncludeBlanks);
                for j := 0 to Pred(SubList.Count) do
                  Result.Add(PList^[I]^.Name + '.' + SubList[j]);
              finally
                SubList.Free;
              end;
            end else begin
              if IncludeValues then begin
                S := GetPropertyValue(PList^[I], C);
                if IncludeBlanks or (S <> '') then
                  Result.Add(PList^[I]^.Name + '=' + S);
              end else
                Result.Add(PList^[I]^.Name);
            end;
          end;
        finally
          FreeMem(PList, LSize);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure SetPropertyValue(C: TObject; PropPath: string; const NewValue: string);
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  SubPropPath: string;
  O: TObject;
begin
  I := pos('=', PropPath);
  if I <> 0 then
    SetLength(PropPath, I - 1);
  PropPath := trim(PropPath);
  if PropPath = '' then
    Exit;
  PropPath := UpperCase(PropPath);
  if C.ClassInfo <> nil then begin
    LCount := GetPropList(C.ClassInfo, tkProperties, nil);
    LSize := LCount * SizeOf(Pointer);
    if LSize > 0 then begin
      GetMem(PList, LSize);
      try
        GetPropList(C.ClassInfo, tkProperties, PList);
        for I := 0 to LCount-1 do begin
          if PList^[I].PropType^.Kind = tkClass then begin
            J := pos('.', PropPath);
            if J <> 0 then begin
              SubPropPath := copy(PropPath, 1, J - 1);
              if SubPropPath = UpperCase(PList^[I]^.Name) then begin
                O := TObject(GetOrdProp(C, PList^[I]));
                SetPropertyValue(O, copy(PropPath, J + 1, MAXINT), NewValue);
                Exit;
              end;
            end;
          end else begin
            if PropPath = UpperCase(PList^[I]^.Name) then begin
              SetPropertyValueLow(PList^[I], C, NewValue);
              Exit;
            end;
          end;
        end;
      finally
        FreeMem(PList, LSize);
      end;
    end;
  end;
  raise Exception.Create('Unknown property:' + PropPath);
end;

procedure TIpHtmlNode.GetAttributes(Target: TStrings; IncludeValues, IncludeBlanks: Boolean);
var
  List : TStringList;
begin
  List := GetPropertyList(Self, IncludeValues, IncludeBlanks);
  try
    Target.Assign(List);
  finally
    List.Free;
  end;
end;

procedure TIpHtmlNode.SetAttributeValue(const AttrName, NewValue: string);
begin
  SetPropertyValue(Self, AttrName, NewValue);
end;

function TIpHtmlNode.ExpParentWidth: Integer;
begin
  if assigned(FParentNode) then
    Result := FParentNode.ExpParentWidth
  else
    Result := MAXINT;
end;

{ TIpHtmlNodeMulti }

constructor TIpHtmlNodeMulti.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FChildren := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  //Maybe this will create some unespected behavior (Owner=nil)
  if Owner <> nil then
    FProps := TIpHtmlProps.Create(FOwner);
end;

destructor TIpHtmlNodeMulti.Destroy;
var
  i : Integer;
begin
  if Owner.Destroying then begin
    for i := 0 to Pred(FChildren.Count) do
      TIpHtmlNode(FChildren[I]).Free;
  end else
    while FChildren.Count > 0 do begin
      TIpHtmlNode(FChildren[FChildren.Count - 1]).Free;
    end;
  FChildren.Free;
  if Assigned(FProps) then FreeAndNil(FProps);
  inherited Destroy;
end;

function TIpHtmlNodeMulti.GetChildNode(Index: Integer): TIpHtmlNode;
begin
  Result := TIpHtmlNode(FChildren[Index]);
end;

function TIpHtmlNodeMulti.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

procedure TIpHtmlNodeMulti.Enqueue;
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).Enqueue;
end;

procedure TIpHtmlNodeMulti.SetProps(const RenderProps: TIpHtmlProps);
var
  i : Integer;
  savedColor, savedBgColor : TColor;
  IsMouseOver: boolean;
  //propb : TIpHtmlPropB;
begin
//DebugLn(ClassName, ':', FParentNode.className, ':', IntToStr(RenderProps.BgColor));
  Props.Assign(RenderProps);
  {$IFDEF IP_LAZARUS}
  if Self.InheritsFrom(TIpHtmlNodeCore)then
    TIpHtmlNodeCore(Self).LoadAndApplyCSSProps;
  {$ENDIF}
//DebugLn(ClassName, ':', FParentNode.className, ':', IntToStr(RenderProps.BgColor));
//  Inc(DebugParseLevel);
  {if Owner.FCurElement <> nil then
  begin
    PageRectToScreen(Owner.FCurElement.WordRect2, vRect);
    IsMouseOver := PtInRect(vRect, Owner.MouseLastPoint);
  end
  else IsMouseOver := False;}

  IsMouseOver := Self = Owner.FHotNode;
  if IsMouseOver then
  begin
    //DebugLn('MouseOver: ', classname);
    Props.DelayCache:=True;
    if Props.HoverColor <> -1 then
    begin
      savedColor := Props.FontColor;
      Props.FontColor := Props.HoverColor;
    end;
    if Props.HoverBgColor <> -1 then
    begin
      savedBgColor := Props.BgColor;
      Props.BgColor := Props.HoverBgColor;
    end;
    Props.DelayCache:=False;
  end;
  for i := 0 to Pred(FChildren.Count) do
  begin
    //propb := Props.PropB;
    TIpHtmlNode(FChildren[i]).SetProps(Props);
    //if propb <> Props.PropB then
    //begin
    //  DebugLn('PropB altered by: ', TIpHtmlNode(FChildren[i]).ClassName);
    //end;
{
    DebugLn(debugDashs , TIpHtmlNode(FChildren[i]).ClassName,
      ':', TIpHtmlNode(FChildren[i]).FParentNode.ClassName,
      ':', IntToStr(RenderProps.BgColor));
}
  end;
  if IsMouseOver then
  begin
    Props.DelayCache:=True;
    if Props.HoverColor <> -1 then Props.FontColor := savedColor;
    if Props.HoverBgColor <> -1 then Props.BgColor := savedBgColor;
    Props.DelayCache:=False;
  end;
//  Dec(DebugParseLevel);
end;

procedure TIpHtmlNodeMulti.ReportDrawRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportDrawRects(M);
end;

procedure TIpHtmlNodeMulti.ReportMapRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportMapRects(M);
end;

procedure TIpHtmlNodeMulti.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
var
  i : Integer;
begin
  inherited;
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).EnumChildren(EnumProc, UserData);
end;

procedure TIpHtmlNodeMulti.AppendSelection(var S: string);
var
  i : Integer;
begin
  inherited;
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).AppendSelection(S);
end;

{ TIpHtmlNodeBODY }

constructor TIpHtmlNodeBODY.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'body';
  {$ENDIF}
  FLink := -1;
  FVLink := -1;
  FALink := -1;
  Owner.Body := Self;
end;

procedure TIpHtmlNodeBODY.Render(const RenderProps: TIpHtmlProps);
var
  MaxX, MaxY: Integer;
  X, Y : Integer;
  P : TPoint;
begin
  if ScaleBitmaps then begin
    Owner.Target.Brush.Color := clWhite;
    Owner.Target.FillRect(Owner.ClientRect);
  end else begin
    {$IFDEF IP_LAZARUS}
    if BackGround = '' then begin
      if BGColor <> -1 then begin
        Owner.Target.Brush.Color := BGColor;
        Owner.Target.FillRect(Owner.ClientRect);
      end else begin
        Owner.Target.Brush.Color := clWhite;
        Owner.Target.FillRect(Owner.ClientRect);
      end;
    end;
    {$ELSE}
    if BackGround = '' then begin
      Owner.Target.Brush.Color := clWhite;
      Owner.Target.FillRect(Owner.ClientRect);
    end;
    if BGColor <> -1 then begin 
      Owner.Target.Brush.Color := BGColor;
      Owner.Target.FillRect(Owner.ClientRect);
    end;
    {$ENDIF}
    if Background <> '' then begin
      if BgPicture = nil then
        Owner.DoGetImage(Self, Owner.BuildPath(Background), BgPicture);
      if (BgPicture <> nil) and (BgPicture.Height>0) and (BgPicture.Width>0) then begin
        MaxX := MaxI2(PageRect.Right, Owner.ClientRect.Right);
        MaxY := MaxI2(PageRect.Bottom, Owner.ClientRect.Bottom);
        Y := 0;
        while (Y <= MaxY{PageRect.Bottom}) do begin
          if (Y < Owner.PageViewRect.Top - BgPicture.Height)
          or (Y > Owner.PageViewRect.Bottom) then
          else begin
            X := 0;
            while (X <= MaxX{PageRect.Right}) do begin
              P := PagePtToScreen(Point(X, Y));
              Owner.Target.Draw(P.X, P.Y, BgPicture.Graphic);
              Inc(X, BgPicture.Width);
            end;
          end;
          Inc(Y, BgPicture.Height);
        end;
      end else begin
        Owner.Target.Brush.Color := clWhite;
        Owner.Target.FillRect(Owner.ClientRect);
      end;
    end;
  end;
  inherited Render(RenderProps);
  {$IFDEF IP_LAZARUS}
  // restore style
  Owner.Target.Brush.Style:=bsSolid;
  {$ENDIF}
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlNodeBODY.LoadAndApplyCSSProps;
var
  LinkProps: TCSSProps;
begin
  Props.DelayCache := True;
  inherited LoadAndApplyCSSProps;
  LinkProps := Owner.CSS.GetPropsObject('a:link', '');
  if (LinkProps <> nil) and (LinkProps.Color <> -1) then
    Link := LinkProps.Color;
  LinkProps := Owner.CSS.GetPropsObject('a:visited', '');
  if (LinkProps <> nil) and (LinkProps.Color <> -1) then
    VLink := LinkProps.Color;
  LinkProps := Owner.CSS.GetPropsObject('a:active', '');
  if (LinkProps <> nil) and (LinkProps.Color <> -1) then
    ALink := LinkProps.Color;
  Props.DelayCache := True;
end;
{$ENDIF}

destructor TIpHtmlNodeBODY.Destroy;
begin
  inherited;
  BgPicture.Free;
end;

procedure TIpHtmlNodeBODY.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  BgPicture.Free;
  BgPicture := NewPicture;
  Invalidate;
end;

procedure TIpHtmlNodeBODY.SetAlink(const Value: TColor);
begin
  if Value <> FAlink then begin
    Falink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetLink(const Value: TColor);
begin
  if Value <> FLink then begin
    FLink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetVlink(const Value: TColor);
begin
  if Value <> FVLink then begin
    FVLink := Value;
    InvalidateSize;
  end;
end;

{ TIpHtml }

procedure TIpHtml.ClearCache;
var
  i : Integer;
begin
  for i := 0 to Pred(PropACache.Count) do begin
    TIpHtmlPropA(PropACache[i]).Free;
  end;
  PropACache.Free;
  for i := 0 to Pred(PropBCache.Count) do
    TIpHtmlPropB(PropBCache[i]).Free;
  PropBCache.Free;
end;

procedure TIpHtml.ResetCache;
var
  i : Integer;
begin
  for i := 0 to Pred(PropACache.Count) do begin
    TIpHtmlPropA(PropACache[i]).FSizeOfSpaceKnown := False;
    TIpHtmlPropA(PropACache[i]).tmHeight := 0;
  end;
end;

procedure TIpHtml.AddWordEntry(const Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  Entry :  PIpHtmlElement;
  L : Integer;
begin
  Entry := NewElement(etWord, Owner);
  Entry.Props := Props;
  Entry.AnsiWord := Value;
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      Inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
  Owner.EnqueueElement(Entry);
end;

procedure TIpHtml.AddWord(Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  P : Integer;
begin
  {$IFDEF IP_LAZARUS}
  if FDocCharset<>'' then
    Value := ConvertEncoding(Value, FDocCharset, 'UTF-8');
  {$ENDIF}
  Value:= EscapeToAnsi(Value);
  P := CharPos(ShyChar, Value);
  if P = 0 then
    AddWordEntry(Value, Props, Owner)
  else begin
    while Value <> '' do begin
      AddWordEntry(copy(Value, 1, P - 1), Props, Owner);
      Delete(Value, 1, P);
      if Value <> '' then
        Owner.EnqueueElement(SoftHyphen);
      P := CharPos(ShyChar, Value);
      if P = 0 then
        P := length(Value) + 1;
    end;
  end;
end;

procedure TIpHtml.InvalidateRect(R: TRect);
begin
  if Assigned(FOnInvalidateRect) then
    FOnInvalidateRect(Self, R);
end;

procedure TIpHtml.Clear;
{- clear any contents}
var
  i : Integer;
begin
  {$IFDEF IP_LAZARUS} //JMN
    {$IFDEF UseGifImageUnit}
    for i := 0 to Pred(GifImages.Count) do
      if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
        TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
    {$ELSE}
    for i := 0 to Pred(AnimationFrames.Count) do
      if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
        TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic).
          AggressiveDrawing := False;
    {$ENDIF}
  {$ELSE}
  for i := 0 to Pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
  {$ENDIF}
  ClearGifQueue;
  FHotNode := nil;
  FHtml.Free;
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
end;

function TIpHtml.NextChar : AnsiChar;
begin
  {$IFDEF IP_LAZARUS}
  Result:=#0;
  {$ENDIF}
  if CharStream.Read(Result, 1) = 0 then
    Result := #0
  else begin
    Inc(GlobalPos);
    if Result = #10 then begin
      Inc(LineNumber);
      LineOffset := 0;
    end else
      Inc(LineOffset);
    {write(Result);}
  end;
end;

procedure TIpHtml.ReportError(const ErrorMsg: string);
begin
  raise Exception.CreateFmt(SHtmlLineError, [ErrorMsg, LineNumber, LineOffset]);
end;

procedure TIpHtml.ReportExpectedError(const ErrorMsg: string);
begin
  ReportError(ErrorMsg + SHtmlExp);
end;

procedure TIpHtml.ReportExpectedToken(const Token: TIpHtmlToken);
var
   n: integer;
begin
  for n:=low(IpHtmlTokens) to high(IpHtmlTokens) do
      if IpHtmlTokens[n].tk = Token then
      begin
         ReportExpectedError(IpHtmlTokens[n].pc);
         break;
      end;
end;

procedure TIpHtml.ReportReferences(Node : TIpHtmlNode);
var
  i : Integer;
  S : string;
begin
  if Node is TIpHtmlNodeA then
    S := Trim(TIpHtmlNodeA(Node).HRef)
  else
  if Node is TIpHtmlNodeAREA then
    S := Trim(TIpHtmlNodeAREA(Node).HRef);

  if (S <> '') then
    ReportReference(S);

  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ReportReferences(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.LoadFromStream(S: TStream);
begin
  DoneLoading := False;
  try
    FHasFrames := False;
    Clear;
    CharStream := S;
    GlobalPos := 0;
    LineNumber := 1;
    LineOffset := 0;
    Parse;
    ReportReferences(HtmlNode);
  finally
    DoneLoading := True;
    FCanPaint := True;
  end;                      
end;

{
procedure TIpHtml.ParseDocType;
begin
  if CurToken = IpHtmlTagDOCTYPE then
    NextToken;
end;
}

function TIpHtml.GetChar : AnsiChar;
var
  Trimming,
  Done: Boolean;
begin
  Trimming := False;
  repeat
    Done := True;
    if (CharSP > 0) then begin
      Dec(CharSP);
      Result := CharStack[CharSP];
    end else begin
      Result := NextChar;
      {if FlagErrors then
        write(Result);}
    end;
    if (InPre = 0) and (CurToken <> IpHtmlTagPRE) then begin
      if (Result <= ' ') and (Result > #0) then begin
        if (Result < ' ') and LastWasClose then begin
          Done := False;
          Trimming := True;
        end else
          if Trimming then
            Done := False
          else
            if LastWasSpace then
              Done := False
            else begin
              Result := ' ';
              LastWasSpace := True;
            end;
      end else
        LastWasSpace := False;
    end;
  until Done;
  LastWasClose := Result = '>';
end;

procedure TIpHtml.PutChar(Ch : AnsiChar);
begin
  if (CharSP >= sizeof(CharStack)) then
    raise EIpHtmlException.Create(SHtmlCharStackOverfl);
  CharStack[CharSP] := Ch;
  Inc(CharSP);
end;

function AnsiToEscape(const S: string): string;
{- returns the string with & escapes}
var
  i : Integer;
  procedure replaceCharBy(newStr: string);
  begin
    Result[i] := '&';
    Insert(newStr, Result, i + 1);
  end;

begin
  Result := S;
  i := length(Result);
  while i > 0 do begin
    case Result[i] of
    ShyChar : replaceCharBy('shy;');
    NbspChar : replaceCharBy('nbsp;');
    '"' : replaceCharBy('quot;');
    '&' : replaceCharBy('amp;');
    '<' : replaceCharBy('lt;');
    '>' : replaceCharBy('gt;');
    end;
    Dec(i);
  end;
end;

procedure TIpHtml.PutToken(Token : TIpHtmlToken);
begin
  if HaveToken then
    raise EIpHtmlException.Create(SHtmlTokenStackOverfl);
  TokenBuffer := Token;
  HaveToken := True;
end;

function TIpHtml.IsWhiteSpace: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to TBW - 1 do
    if TokenStringBuf[i] > ' ' then
      Exit;
  Result := True;
end;

procedure TrimFormatting(const S: string; Target: PAnsiChar; PreFormatted: Boolean = False);
var
  R, W : Integer;

  procedure CopyChar(ch: AnsiChar);
  begin
    Target[w] := ch;
    Inc(w);
  end;

begin
  r := 1;
  w := 0;
  while r <= length(S) do begin
    case S[r] of
    #0..#8, #11..#12, #14..#31 :
      ;
    #9 :
      if PreFormatted then
        CopyChar(' ');
    #13 :
      if PreFormatted then
        CopyChar(LF);
    #10 :
      if PreFormatted then begin
        if (w = 0) or (Target[w-1] <> LF) then
          CopyChar(LF);
      end
      else begin
        if w > 1 then
          CopyChar(' ');
      end;
    ' ' :
      if PreFormatted or (w = 0) or (Target[w-1] <> ' ') then
        CopyChar(' ');
    else
      CopyChar(S[r]);
    end;
    Inc(r);
  end;
  Target[w] := #0;
end;

function TIpHtml.GetTokenString: string;
begin
  TokenStringBuf[TBW] := #0;
  Result := StrPas(TokenStringBuf);
end;

procedure TIpHtml.ClearParmValueArray;
var
  n: TIpHtmlAttributesSet;
begin
  for n:=Low(ParmValueArray) to High(ParmValueArray) do
    setLength(ParmValueArray[n],0);
end;

procedure TIpHtml.ParmValueArrayAdd(const sName, sValue: string);
var
  vFirst, vLast, vPivot: Integer;
begin
  vFirst := Ord(Low(TIpHtmlAttributesSet)); //Sets the first item of the range
  vLast  := Ord(High(TIpHtmlAttributesSet)); //Sets the last item of the range

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (vFirst <= vLast) do
  begin
    //Gets the middle of the selected range
    vPivot := (vFirst + vLast) div 2;
    //Compares the String in the middle with the searched one
    if TIpHtmlAttributesNames[TIpHtmlAttributesSet(vPivot)] = sName then
    begin
      ParmValueArray[TIpHtmlAttributesSet(vPivot)] := sValue;
      Exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if TIpHtmlAttributesNames[TIpHtmlAttributesSet(vPivot)] > sName then
      vLast := Pred(vPivot)//else select the second half
    else
      vFirst := Succ(vPivot);
  end;
end;

function TIpHtml.HtmlTokenListIndexOf(const TokenString: PAnsiChar): integer;
var
  vFirst: Integer;
  vLast: Integer;
  vPivot: Integer;
  vicmp: integer;
begin
  vFirst  := Low(IpHtmlTokens); //Sets the first item of the range
  vLast   := High(IpHtmlTokens); //Sets the last item of the range
  Result  := -1; //Initializes the Found flag (Not found yet)

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (vFirst <= vLast) do
  begin
    //Gets the middle of the selected range
    vPivot := (vFirst + vLast) div 2;
    //Compares the String in the middle with the searched one
    vicmp := strcomp(IpHtmlTokens[vPivot].pc, TokenString);
    if vicmp = 0 then
    begin
      Result  := vPivot;
      exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if vicmp > 0 then
      vLast := vPivot - 1    //else select the second half
    else
      vFirst := vPivot + 1;
  end;
end;

procedure TIpHtml.NextToken;
var
  ParmName : string;
  {ParmBuf : array[0..4095] of AnsiChar;}
  PBW : Integer;
  i : Integer;
  Ctl,
  InValue, InQuote, InAttr, SeenEqual,
  SeenQuotes, Done, EndFound : Boolean;
  QuoteChar : AnsiChar;
  Ch : AnsiChar;

  procedure AddParmChar(const Ch: AnsiChar);
  begin
    if PBW >= ParmBufSize - 1 then begin
      Inc(ParmBufSize, 4096);
      ReallocMem(ParmBuf, ParmBufSize);
    end;
    ParmBuf[PBW] := Ch;
    Inc(PBW);
  end;

  function ParmString: string;
  begin
    if PBW = 0 then
      Result := ''
    else begin
      ParmBuf[PBW] := #0;
      Result := StrPas(ParmBuf);
      PBW := 0;
    end;
  end;

  procedure AddTokenChar(const Ch: AnsiChar);
  begin
    TokenStringBuf[TBW] := Ch;
    Inc(TBW);
  end;

begin
  if HaveToken then begin
    CurToken := TokenBuffer;
    HaveToken := False;
    Exit;
  end;
  QuoteChar := ' ';
  repeat
    TBW := 0;
    PBW := 0;
    ClearParmValueArray;
    Ch := GetChar;
    if Ch = #0 then begin
      CurToken := IpHtmlTagEof;
      Exit;
    end;
    if Ch = '<' then begin
      Ch := GetChar;
      if Ch = '!' then begin
        if GetChar = '-' then begin
          if GetChar <> '-' then
            if FlagErrors then
              ReportError(SHtmlDashExp);
          Ch := GetChar;
          repeat
            while Ch <> '-' do begin
              if Ch = #0 then
                break;
              Ch := GetChar;
            end;
            if (Ch = #0) then
              break
            else begin
              Ch := GetChar;
              if Ch = #0 then
                break;
              if Ch = '-' then begin
                Ch := GetChar;
                while (Ch = '-') do
                  Ch := GetChar;
                {if (Ch = #0) or (Ch = '>') then
                  break;}
                while not (Ch in [#0,'>']) do
                  Ch := GetChar;
                break;
              end;
            end;
          until false;
          CurToken := IpHtmlTagComment;
        end else begin
          Ch := GetChar;
          while Ch <> '>' do
            Ch := GetChar;
          CurToken := IpHtmlTagComment;
        end;
      end else begin
        while Ch <> '>' do begin
          if Ch <= ' ' then begin
            Ch := ' ';
            break;
          end;
          if Ch in [#33..#255] then
            AddTokenChar(UpCase(Ch));
          Ch := GetChar;
        end;
        if Ch = ' ' then begin
          Ch := GetChar;
          {list :== [attr]* ">"}
          {attr :== [" "]* attr-name [attr-value]}
          {attr-value :== [" "]* "=" [" "]* value}
          {value :== ['"']* string ['"']*}
          InAttr := False;
          InValue := False;
          InQuote := False;
          SeenEqual := False;
          SeenQuotes := False;
          ParmName := '';
          PBW := 0;
          while True do begin
            case Ch of
            #0 : break;
            #1..#31 :
              if InAttr then begin
                InAttr := False;
                ParmName := ParmString;
                SeenEqual := False;
              end else
              if InValue then begin
                if ParmName <> '' then begin
                  ParmValueArrayAdd(UpperCase(ParmName), ParmString);
                  ParmName := '';
                end;
                InValue := False;
                SeenEqual := False;
                SeenQuotes := False;
              end;
            ' ' :
              if InQuote then
                AddParmChar(' ')
              else
              if InAttr then begin
                InAttr := False;
                ParmName := ParmString;
                SeenEqual := False;
              end else
              if InValue then begin
                if ParmName <> '' then begin
                  ParmValueArrayAdd(UpperCase(ParmName), ParmString);
                  ParmName := '';
                end;
                InValue := False;
                SeenEqual := False;
                SeenQuotes := False;
              end;
            '''' :
              if InQuote then
                if QuoteChar = '''' then
                  InQuote := False
                else
                  AddParmChar('''')
              else begin
                InQuote := True;
                SeenQuotes := True;
                QuoteChar := '''';
              end;
            '"' :
              if InQuote then
                if QuoteChar = '"' then
                  InQuote := False
                else
                  AddParmChar('"')
              else begin
                InQuote := True;
                SeenQuotes := True;
                QuoteChar := '"';
              end;
            '<', '>' :
              begin
                if InQuote then
                  AddParmChar(Ch)
                else begin
                  if InValue then begin
                    if ParmName <> '' then begin
                      ParmValueArrayAdd(UpperCase(ParmName), ParmString);
                      ParmName := '';
                    end;
                  end;
                  break;
                end;
              end;
            '=' :
              begin
                SeenEqual := True;
                if InAttr then begin
                  ParmName := ParmString;
                  InAttr := False;
                end else
                  if InValue then
                    AddParmChar(Ch)
              end;
            else
              if InAttr or InValue then
                AddParmChar(Ch)
              else
                if SeenEqual and (InQuote or not SeenQuotes) then begin
                  InValue := True;
                  AddParmChar(Ch);
                end else begin
                  if (ParmName <> '') and not SeenQuotes then begin
                    ParmName := UpperCase(ParmName);
                    ParmValueArrayAdd(ParmName, ParmName);
                  end;
                  ParmName := '';
                  AddParmChar(Ch);
                  SeenEqual := False;
                  SeenQuotes := False;
                  InValue := False;
                  InAttr := True;
                end;
            end;
            Ch := GetChar;
          end;
          if InAttr then begin
            ParmName := UpperCase(ParmString);
            if (ParmName <> '') then begin
              ParmValueArrayAdd(ParmName, ParmName);
            end;
          end;
        end;

        { Check if this is a token of the form <tok/> }
        if (TBW > 0) and (TokenStringBuf[TBW - 1] = '/') then begin
          {It is, set EndFound flag and convert to normal open token}
          EndFound := True;
          Dec(TBW);
        end else
          EndFound := False;
        TokenStringBuf[TBW] := #0;
        CurToken := IpHtmlTagUnknown;
        i := HtmlTokenListIndexOf(TokenStringBuf);
        if i <> -1 then
          CurToken := IpHtmlTokens[i].tk;

        {If the token was a single terminated token ( <tok/>
         as opposed to normal a <tok></tok> sequence), we fake
         it by pushing a close token to match the open token
         which was mangled above where EndFound was set.}

        if (CurToken <> IpHtmlTagUnknown) and EndFound then
          if succ(CurToken) in IpEndTokenSet then
            PutToken(succ(CurToken));
      end;
    end else begin
      CurToken := IpHtmlTagText;
      repeat
        Done := True;
        Ctl := False;
        while Ch <> '<' do begin
          case Ch of
          #0 :
            break;
          #10,#13 :
            begin
              Ctl := True;
              if InPre > 0 then
                AddTokenChar(Ch);
            end
          else
            AddTokenChar(Ch);
          end;
          Ch := GetChar;
        end;
        if Ch <> #0 then begin
          Ch := GetChar;
          while (Ch > #0) and (Ch < ' ') do
            Ch := GetChar;
          case Ch of
          '/', '!', 'a'..'z','A'..'Z' :
            begin
              PutChar(Ch);
              PutChar('<');
            end
          else
            begin
              AddTokenChar('<');
              AddTokenChar(Ch);
              Done := False;
              Ch := GetChar;
            end;
          end;
        end;
        if (InPre = 0) and Ctl
        and IsWhiteSpace then
          CurToken := IpHtmlTagCOMMENT;
      until Done;
    end;
    //eat script blocks that could confuse the parsing
    //example www.sqlite.org has javascript to write dynamic
    //content inside a table
    if CurToken = IpHtmlTagSCRIPT then ParseScript(FHtml,[]);
  until
    (CurToken <> IpHtmlTagCOMMENT)
  and ((CurToken <> IpHtmlTagText) or (InBlock > 0) or (InPre > 0)
    or not IsWhiteSpace);
end;

procedure TIpHtml.NextRealToken;
begin
  repeat
    NextToken;
  until CurToken <> IpHtmlTagText;
end;

procedure TIpHtml.NextNonBlankToken;
begin
  repeat
    NextToken;
  until (CurToken <> IpHtmlTagText)
  or not IsWhiteSpace;
end;

procedure TIpHtml.SkipTextTokens;
begin
  while CurToken = IpHtmlTagText do
    NextToken;
end;

procedure TIpHtml.EnsureClosure(const EndToken : TIpHtmlToken;
  const EndTokens : TIpHtmlTokenSet);
begin
  if CurToken = EndToken then
    NextToken
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseTitle(Parent: TIpHtmlNode);
var
  B : PAnsiChar;
begin
  FTitleNode := TIpHtmlNodeTITLE.Create(Parent);
  NextToken;
  if CurToken = IpHtmlTagText then begin
    Getmem(B, length(GetTokenString) + 1);
    try
      TrimFormatting(EscapeToAnsi(GetTokenString), B);
      FTitleNode.Title := B;
    finally
      Freemem(B);
    end;
    NextToken;
  end;
  if CurToken = IpHtmlTagTITLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagTITLEend);
end;

procedure TIpHtml.ParseStyle(ParentNode : TIpHtmlNode);
var
  CurStyle : TIpHtmlNodeSTYLE;
begin
  CurStyle := TIpHtmlNodeSTYLE.Create(ParentNode);
  with CurStyle do begin
    Media := FindAttribute(htmlAttrMEDIA);
    Title := FindAttribute(htmlAttrTITLE);
    {$IFDEF IP_LAZARUS}
    Type_ := FindAttribute(htmlAttrTYPE);
    {$ENDIF}
  end;
  NextToken;
  if CurToken <> IpHtmlTagSTYLEend then begin
    {$IFDEF IP_LAZARUS}
    if (CurToken=IpHtmlTagText) and
      (AnsiCompareText(CurStyle.Type_ , 'text/css')=0) then
      ParseStyleSheet(CurStyle, GetTokenString);
    {$ENDIF}
    ParseText([IpHtmlTagSTYLEend], CurStyle);
  end;
  if CurToken = IpHtmlTagSTYLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagSTYLEend);
end;

procedure TIpHtml.ParseScript(Parent : TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
begin
  TIpHtmlNodeSCRIPT.Create(Parent);
  NextToken;
  if CurToken <> IpHtmlTagScriptEnd then
    repeat
      NextToken;
    until (CurToken = IpHtmlTagSCRIPTend)
      or (CurToken in EndTokens);
  EnsureClosure(IpHtmlTagSCRIPTend, EndTokens);
end;

procedure TIpHtml.ParseNoscript(Parent : TIpHtmlNode);
var
  CurScript : TIpHtmlNodeNOSCRIPT;
begin
  CurScript := TIpHtmlNodeNOSCRIPT.Create(Parent);
  with CurScript do begin
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurScript, [IpHtmlTagNOSCRIPTend]);
  if CurToken = IpHtmlTagNOSCRIPTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOSCRIPTend);
end;

procedure TIpHtml.ParseIsIndex;
begin
  IndexPhrase := FindAttribute(htmlAttrPROMPT);
  {IsIndexPresent := IndexPhrase <> '';}
  NextToken;
end;

procedure TIpHtml.ParseBase;
begin
  {Base := FindAttribute(htmlAttrHREF);}
  NextToken;
end;

procedure TIpHtml.ParseMeta;
{$IFDEF IP_LAZARUS}
var
  i,j: Integer;
{$ENDIF}
begin
  with TIpHtmlNodeMETA.Create(Parent) do begin
    HttpEquiv := FindAttribute(htmlAttrHTTP_EQUIV);
    Name := FindAttribute(htmlAttrNAME);
    Content := FindAttribute(htmlAttrCONTENT);
    {$IFDEF IP_LAZARUS}
    if not FHasBOM then begin
      j := pos('charset=', lowercase(Content));
      if j>0 then begin
        j := j+8;
        i := j;
        while (j<=Length(Content)) do begin
          if Content[j] in [' ',';','"',','] then
            break;
          inc(j);
        end;
        fDocCharset := copy(content, i, j-i);
      end;
    end;
    {$ENDIF}
    Scheme := FindAttribute(htmlAttrSCHEME);
  end;
  NextToken;
end;

procedure TIpHtml.ParseLink(Parent : TIpHtmlNode);
begin
  with TIpHtmlNodeLINK.Create(Parent) do begin
    HRef := FindAttribute(htmlAttrHREF);
    Rel := FindAttribute(htmlAttrREL);
    Rev := FindAttribute(htmlAttrREV);
    Title := FindAttribute(htmlAttrTITLE);
    {$IFDEF IP_LAZARUS}
    Type_ := LowerCase(FindAttribute(htmlAttrTYPE));
    if (LowerCase(Rel) = 'stylesheet') and (Type_ = 'text/css') then
      ParseStyleSheet(Parent, Href);
    {$ENDIF}
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseHeadItems(Parent : TIpHtmlNode);
begin
  while not (CurToken in
    [IpHtmlTagEOF, IpHtmlTagHEADend, IpHtmlTagFRAMESET, IpHtmlTagBODY]) do begin
    case CurToken of
    IpHtmlTagTITLE :
      ParseTitle(Parent);
    IpHtmlTagSTYLE :
      ParseStyle(Parent);
    IpHtmlTagSCRIPT :
      ParseScript(Parent, [IpHtmlTagEOF]);
    IpHtmlTagNOSCRIPT :
      ParseNoscript(Parent);
    IpHtmlTagISINDEX :
      ParseIsIndex;
    IpHtmlTagBASE :
      ParseBase;
    IpHtmlTagMETA :
      ParseMeta(Parent);
    IpHtmlTagLINK :
      ParseLink(Parent);
    else
      {unknown}
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseHead(Parent : TIpHtmlNode);
{$IFDEF IP_LAZARUS}
var
  Lst: TStringList;
{$ENDIF}
begin
  {lead token is optional}
  if CurToken = IpHtmlTagHEAD then begin
    NextToken;
    ParseHeadItems(TIpHtmlNodeHEAD.Create(Parent));
    if CurToken = IpHtmlTagHEADend then
      NextToken;
  end;
  {$IFDEF IP_LAZARUS}
  Lst := TStringList.Create;
  GetSupportedEncodings(Lst);
  if Lst.IndexOf(FDocCharset)=0 then
    FDocCharset := '';
  Lst.Free;
  {$ENDIF}
end;

procedure TIpHtml.ParseFont(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurFONT : TIpHtmlNodeFONT;
begin
  CurFONT := TIpHtmlNodeFONT.Create(Parent);
  with CurFONT do begin
    Face := FindAttribute(htmlAttrFACE);
    Size.Free;
    Size := ParseRelSize{('+0')};
    Size.OnChange := SizeChanged;
    Color := ColorFromString(FindAttribute(htmlAttrCOLOR));
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurFONT, EndTokens + [IpHtmlTagFONTend]);
  EnsureClosure(IpHtmlTagFONTend, EndTokens);
end;

procedure TIpHtml.ParsePre(ParentNode : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodePRE;
begin
  CurContainer := TIpHtmlNodePRE.Create(ParentNode);
  CurContainer.ParseBaseProps(Self);
  Inc(InPre);
  NextToken;
  {Inc(InPre);}
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagPREend]);
  Dec(InPre);
  EnsureClosure(IpHtmlTagPREend, EndTokens);
end;

procedure TIpHtml.ParseText(const EndTokens : TIpHtmlTokenSet; Parent: TIpHtmlNode);
var
  CurContainer : TIpHtmlNodeText;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagEof :
      Exit;
    {IpHtmlTagFont :
      begin
        ParseFont(Parent, EndTokens);
      end;}
    IpHtmlTagText :
      begin
        CurContainer := TIpHtmlNodeText.Create(Parent);
        if CurContainer=nil then ;
        CurContainer.FEscapedText := GetTokenString;
        NextToken;
      end;
    else
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseHeader(Parent : TIpHtmlNode; EndToken : TIpHtmlToken; Size : Integer);
var
  NewHeader : TIpHtmlNodeHeader;
begin
  NewHeader := TIpHtmlNodeHeader.Create(Parent);
  {$IFDEF IP_LAZARUS}
  NewHeader.FElementName := 'h'+IntToStr(Size);
  {$ENDIF}
  NewHeader.ParseBaseProps(Self);
  NewHeader.Size := Size;
  NewHeader.Align := ParseAlignment;
  NextToken;
  ParseBodyText(NewHeader, [EndToken]);
  if CurToken = EndToken then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseParagraph(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  NewPara : TIpHtmlNodeP;
begin
  NewPara := TIpHtmlNodeP.Create(Parent);
  NewPara.ParseBaseProps(Self);
  NewPara.Align := ParseAlignment;
  NextToken;
  ParseBodyText(NewPara, EndTokens + [IpHtmlTagPend, IpHtmlTagP, IpHtmltagTABLE]);
  if CurToken = IpHtmlTagPend then
    NextToken
  else
  if CurToken in (EndTokens + [IpHtmlTagP, IpHtmltagTABLE]) then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagPend);
end;

procedure TIpHtml.ParseAddress(Parent : TIpHtmlNode);
var
  NewPara : TIpHtmlNodeADDRESS;
begin
  NewPara := TIpHtmlNodeADDRESS.Create(Parent);
  NewPara.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(NewPara, [IpHtmlTagADDRESSend]);
  if CurToken = IpHtmlTagADDRESSend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagADDRESSend);
end;

procedure TIpHtml.ParseListItems(Parent : TIpHtmlNodeCore;
        EndToken: TIpHtmlToken; const EndTokens : TIpHtmlTokenSet;
        DefaultListStyle : TIpHtmlULType);
var
  NewListItem : TIpHtmlNodeLI;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagLI :
      begin
        NewListItem := TIpHtmlNodeLI.Create(Parent);
        NewListItem.ParseBaseProps(Self);
        {NewListItem.DefListType := DefaultListStyle;}
        NewListItem.ListType := ParseULStyle(DefaultListStyle);
        NewListItem.Value := ParseInteger(htmlAttrVALUE, -1);
        NewListItem.Compact := ParseBoolean(htmlAttrCOMPACT);
        NextToken;
        ParseBodyText(NewListItem,
                      EndTokens + [EndToken, IpHtmlTagLI, IpHtmlTagLIend] -
                                  [IpHtmlTagP, IpHtmlTagPend]);
        if CurToken = IpHtmlTagLIend then
          NextToken;
        SkipTextTokens;
      end;
    else
      ParseBodyText(Parent, EndTokens + [EndToken, IpHtmlTagLI]);
    end;
  end;
end;

procedure TIpHtml.ParseUnorderedList(Parent: TIpHtmlNode;
  EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  NewList : TIpHtmlNodeList;
begin
  case Pred(EndToken) of
  IpHtmlTagDIR : NewList := TIpHtmlNodeDIR.Create(Parent);
  IpHtmlTagMENU : NewList := TIpHtmlNodeMENU.Create(Parent);
  else {IpHtmlTagUL : }NewList := TIpHtmlNodeUL.Create(Parent);
  end;
  NewList.ParseBaseProps(Self);
  case ListLevel of
  0 : NewList.ListType := ParseULStyle(ulDisc);
  1 : NewList.ListType := ParseULStyle(ulCircle);
  else
    NewList.ListType := ParseULStyle(ulSquare);
  end;
  NewList.Compact := ParseBoolean(htmlAttrCOMPACT);
  NextToken;
  Inc(ListLevel);
  ParseListItems(NewList,
                 EndToken, EndTokens + [EndToken] - [IpHtmlTagP, IpHtmlTagLI],
                 NewList.ListType);
  Dec(ListLevel);
  EnsureClosure(EndToken, EndTokens);
end;

procedure TIpHtml.ParseOrderedList(Parent: TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
var
  NewList : TIpHtmlNodeOL;
begin
  NewList := TIpHtmlNodeOL.Create(Parent);
  NewList.Style := ParseOLStyle(olArabic);
  NewList.Start := ParseInteger(htmlAttrSTART, 1);
  NewList.Compact := ParseBoolean(htmlAttrCOMPACT);
  NextToken;
  ParseListItems(NewList, IpHtmlTagOLend, EndTokens + [IpHtmlTagOLend], ulDisc);
  EnsureClosure(IpHtmlTagOLend, EndTokens);
end;

const
  TIpHtmlButtonTypeNames : array[TIpHtmlButtonType] of string = (
    'SUBMIT','RESET','BUTTON');
  TIpHtmlInputTypeNames : array[TIpHtmlInputType] of string = (
    'TEXT', 'PASSWORD', 'CHECKBOX', 'RADIO',
    'SUBMIT', 'RESET', 'FILE', 'HIDDEN', 'IMAGE', 'BUTTON');

function TIpHtml.ParseInputType : TIpHtmlInputType;
var
  S : string;
begin
  Result := hitText;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if (length(S) = 0) or (S = 'TEXTAREA') then
  else
  begin
    for Result:=low(TIpHtmlInputType) to high(TIpHtmlInputType) do
      if S = TIpHtmlInputTypeNames[Result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

function TIpHtml.ParseButtonType : TIpHtmlButtonType;
var
  S : string;
begin
  Result := hbtSubmit;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if length(S) > 0 then
  begin
    for Result:=low(TIpHtmlButtonType) to high(TIpHtmlButtonType) do
      if S = TIpHtmlButtonTypeNames[Result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

procedure TIpHtml.ParseFormFields(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
var
  CurSelect : TIpHtmlNodeSELECT;
  CurTextArea : TIpHtmlNodeTEXTAREA;
  CurButton : TIpHtmlNodeBUTTON;
  CurOptGroup : TIpHtmlNodeOPTGROUP;
  CurLabel : TIpHtmlNodeLABEL;
  CurFieldset : TIpHtmlNodeFIELDSET;
  CurLegend : TIpHtmlNodeLEGEND;
  CurOption : TIpHtmlNodeOPTION;
  {$IFDEF IP_LAZARUS}
  CurInput : TIpHtmlNodeINPUT;
  {$ENDIF}
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagINPUT :
      begin
        CurInput := TIpHtmlNodeINPUT.Create(Parent);
        {$IFDEF IP_LAZARUS}
        FTabList.Add(CurInput);
        {$ENDIF}
        with CurInput do begin
          ParseBaseProps(Self);
          InputType := ParseInputType;
          Name := FindAttribute(htmlAttrNAME);
          Value := FindAttribute(htmlAttrVALUE);
          Checked := ParseBoolean(htmlAttrCHECKED);
          Size := ParseInteger(htmlAttrSIZE, -1);
          MaxLength := ParseInteger(htmlAttrMAXLENGTH, -1);
          Src := FindAttribute(htmlAttrSRC);
          Align := ParseImageAlignment(hiaBottom);
          Disabled := ParseBoolean(htmlAttrDISABLED);
          ReadOnly := ParseBoolean(htmlAttrREADONLY);
          Alt := FindAttribute(htmlAttrALT);
          TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
        end;
        NextToken;
      end;
    IpHtmlTagBUTTON :
      begin
        CurButton := TIpHtmlNodeBUTTON.Create(Parent);
        {$IFDEF IP_LAZARUS}
        FTabList.Add(CurButton);
        {$ENDIF}
        with CurButton do begin
          ParseBaseProps(Self);
          ButtonType := ParseButtonType;
          Name := FindAttribute(htmlAttrNAME);
          Value := FindAttribute(htmlAttrVALUE);
          Disabled := ParseBoolean(htmlAttrDISABLED);
          TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
        end;
        NextToken;
        ParseBodyText(CurButton, EndTokens + [IpHtmlTagBUTTONend]);
        if CurToken = IpHtmlTagBUTTONend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagBUTTONend);
      end;
    IpHtmlTagSELECT :
      begin
        CurSelect := TIpHtmlNodeSELECT.Create(Parent);
        with CurSelect do begin
          Name := FindAttribute(htmlAttrNAME);
          Size := ParseInteger(htmlAttrSIZE, -1);
          Width := ParseInteger(htmlAttrWIDTH, -1);
          ParseBaseProps(Self);
          Multiple := ParseBoolean(htmlAttrMULTIPLE);
          ComboBox := ParseBoolean(htmlAttrCOMBOBOX);
          Disabled := ParseBoolean(htmlAttrDISABLED);
          TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
          Alt := FindAttribute(htmlAttrALT);
        end;
        NextNonBlankToken;
        repeat
          case CurToken of
          IpHtmlTagOPTION :
            begin
              CurOption := TIpHtmlNodeOPTION.Create(CurSelect);
              with CurOption do begin
                ParseBaseProps(Self);
                Selected := ParseBoolean(htmlAttrSELECTED);
                Value := FindAttribute(htmlAttrVALUE);
                Disabled := ParseBoolean(htmlAttrDISABLED);
                OptionLabel := FindAttribute(htmlAttrLABEL);
              end;
              NextNonBlankToken;
              ParseText(EndTokens +
                        [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend],
                        CurOption);
              if CurToken = IpHtmlTagOPTIONend then
                NextNonBlankToken;
            end;
          IpHtmlTagOPTGROUP :
            begin
              CurOptGroup := TIpHtmlNodeOPTGROUP.Create(CurSelect);
              with CurOptGroup do begin
                ParseBaseProps(Self);
                Disabled := ParseBoolean(htmlAttrDISABLED);
                GroupLabel := FindAttribute(htmlAttrLABEL);
              end;
              NextNonBlankToken;
              while CurToken = IpHtmlTagOPTION do begin
                CurOption := TIpHtmlNodeOPTION.Create(CurOptGroup);
                {$IFDEF IP_LAZARUS}
                FTabList.Add(CurOption);
                {$ENDIF}
                with CurOption do begin
                  ParseBaseProps(Self);
                  Selected := ParseBoolean(htmlAttrSELECTED);
                  Value := FindAttribute(htmlAttrVALUE);
                  Disabled := ParseBoolean(htmlAttrDISABLED);
                  OptionLabel := FindAttribute(htmlAttrLABEL);
                end;
                NextNonBlankToken;
                ParseText(EndTokens +
                          [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend],
                          CurOption);
                if CurToken = IpHtmlTagOPTIONend then
                  NextNonBlankToken;
              end;
              if CurToken = IpHtmlTagOPTGROUPend then
                NextNonBlankToken
              else
              if CurToken = IpHtmlTagOPTGROUP then
              else
              if CurToken = IpHtmlTagOPTION then
              else
              if CurToken = IpHtmlTagSELECTend then
              else
                if FlagErrors then
                  ReportExpectedToken(IpHtmlTagOPTGROUPend);
            end;
          else
            break;
          end;
        until False;
        if CurToken = IpHtmlTagSELECTend then
          NextNonBlankToken;
      end;
    IpHtmlTagTEXTAREA :
      begin
        CurTextArea := TIpHtmlNodeTEXTAREA.Create(Parent);
        {$IFDEF IP_LAZARUS}
        FTabList.Add(CurTextArea);
        {$ENDIF}
        with CurTextArea do begin
          Name := FindAttribute(htmlAttrNAME);
          Rows := ParseInteger(htmlAttrROWS, 20);
          Cols := ParseInteger(htmlAttrCOLS, 20);
          ParseBaseProps(Self);
          Disabled := ParseBoolean(htmlAttrDISABLED);
          ReadOnly := ParseBoolean(htmlAttrREADONLY);
          TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
          Alt := FindAttribute(htmlAttrALT);
        end;
        NextToken;
        ParseText([IpHtmlTagTEXTAREAend], CurTextArea);
        if CurToken = IpHtmlTagTEXTAREAend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagTEXTAREAend);
      end;
    IpHtmlTagLABEL :
      begin
        CurLabel := TIpHtmlNodeLABEL.Create(Parent);
        with CurLabel do begin
          ParseBaseProps(Self);
          LabelFor := FindAttribute(htmlAttrLABEL);
        end;
        NextToken;
        ParseBodyText(CurLabel, [IpHtmlTagLABELend]);
        if CurToken = IpHtmlTagLABELend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagLABELend);
      end;
    IpHtmlTagFIELDSET :
      begin
        CurFieldset := TIpHtmlNodeFIELDSET.Create(Parent);
        with CurFieldset do
          ParseBaseProps(Self);
        NextToken;
        ParseFormFields(CurFieldSet, EndTokens + [IpHtmlTagFIELDSETend]);
        if CurToken = IpHtmlTagFIELDSETend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagFIELDSETend);
      end;
    IpHtmlTagLEGEND :
      begin
        CurLegend := TIpHtmlNodeLEGEND.Create(Parent);
        with CurLegend do begin
          ParseBaseProps(Self);
        end;
        NextToken;
        ParseBodyText(CurLegend, [IpHtmlTagLEGENDend]);
        if CurToken = IpHtmlTagLEGENDend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagLEGENDend);
      end;
    else
      Exit;
    end;
  end;
end;

procedure TIpHtml.ParseForm(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  NewForm : TIpHtmlNodeFORM;
begin
  NewForm := TIpHtmlNodeFORM.Create(Parent);
  with NewForm do begin
    Action := FindAttribute(htmlAttrACTION);
    Method := ParseMethod;
    Enctype := FindAttribute(htmlAttrENCTYPE);
    Name := FindAttribute(htmlAttrNAME);
    AcceptCharset := FindAttribute(htmlAttrACCEPT_CHARSET);
    Accept := FindAttribute(htmlAttrACCEPT);
    if Enctype = '' then
      Enctype := 'application/x-www-form-urlencoded';
    if AcceptCharset = '' then
      AcceptCharset := 'UNKNOWN';
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(NewForm, EndTokens + [IpHtmlTagFORMend]);
  EnsureClosure(IpHtmlTagFORMend, EndTokens);
end;

procedure TIpHtml.ParseDefListItems(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurDT : TIpHtmlNodeDT;
  CurDD : TIpHtmlNodeDD;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagDT :
      begin
        CurDT := TIpHtmlNodeDT.Create(Parent);
        CurDT.ParseBaseProps(Self);
        NextToken;
        ParseBodyText(CurDT, [IpHtmlTagDD, IpHtmlTagDTend] + EndTokens);
        if CurToken = IpHtmlTagDTend then
          NextToken;
      end;
    IpHtmlTagDD :
      begin
        CurDD := TIpHtmlNodeDD.Create(Parent);
        CurDD.ParseBaseProps(Self);
        NextToken;
        ParseBodyText(CurDD, [IpHtmlTagDT, IpHtmlTagDDend] + EndTokens);
        if CurToken = IpHtmlTagDDend then
          NextToken;
      end;
    else
      ParseBodyText(Parent, EndTokens + [IpHtmlTagDT, IpHtmlTagDD]);
    end;
  end;
end;

procedure TIpHtml.ParseDefinitionList(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  NewDL : TIpHtmlNodeDL;
begin
  NewDL := TIpHtmlNodeDL.Create(Parent);
  NewDL.ParseBaseProps(Self);
  NewDL.Compact := ParseBoolean(htmlAttrCOMPACT);
  NextToken;
  ParseDefListItems(NewDL, EndTokens + [IpHtmlTagDLend]);
  EnsureClosure(IpHtmlTagDLend, EndTokens);
end;

procedure TIpHtml.ParseDIV(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurDIV : TIpHtmlNodeDIV;
begin
  CurDIV := TIpHtmlNodeDIV.Create(Parent);
  with CurDIV do begin
    Align := ParseAlignment;
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurDIV, EndTokens + [IpHtmlTagDIVend]);
  EnsureClosure(IpHtmlTagDIVend, EndTokens);
end;

procedure TIpHtml.ParseSPAN(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurSPAN : TIpHtmlNodeSPAN;
begin
  CurSPAN := TIpHtmlNodeSPAN.Create(Parent);
  with CurSPAN do begin
    Align := ParseAlignment;
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurSPAN, EndTokens + [IpHtmlTagSPANend]);
  EnsureClosure(IpHtmlTagSPANend, EndTokens);
end;

procedure TIpHtml.ParseCENTER(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haCenter;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagCENTERend]);
  EnsureClosure(IpHtmlTagCENTERend, EndTokens);
end;

procedure TIpHtml.ParseLEFT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haLeft;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagLEFTend]);
  EnsureClosure(IpHtmlTagLEFTend, EndTokens);
end;

procedure TIpHtml.ParseRIGHT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haRight;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagRIGHTend]);
  EnsureClosure(IpHtmlTagRIGHTend, EndTokens);
end;

procedure TIpHtml.ParseBLINK(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurBlink : TIpHtmlNodeBLINK;
begin
  CurBlink := TIpHtmlNodeBLINK.Create(Parent);
  NextToken;
  ParseBodyText(CurBlink, EndTokens + [IpHtmlTagBLINKend]);
  EnsureClosure(IpHtmlTagBLINKend, EndTokens);
end;

procedure TIpHtml.ParseBLOCKQUOTE(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeBLOCKQUOTE;
begin
  BQ := TIpHtmlNodeBLOCKQUOTE.Create(Parent);
  BQ.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagBLOCKQUOTEend]);
  EnsureClosure(IpHtmlTagBLOCKQUOTEend, EndTokens);
end;

procedure TIpHtml.ParseQ(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeQ;
begin
  BQ := TIpHtmlNodeQ.Create(Parent);
  BQ.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagQend]);
  EnsureClosure(IpHtmlTagQend, EndTokens);
end;

procedure TIpHtml.ParseINS(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeINS;
begin
  BQ := TIpHtmlNodeINS.Create(Parent);
  BQ.ParseBaseProps(Self);
  BQ.Cite := FindAttribute(htmlAttrCITE);
  BQ.Datetime := FindAttribute(htmlAttrDATETIME);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagINSend]);
  EnsureClosure(IpHtmlTagINSend, EndTokens);
end;

procedure TIpHtml.ParseDEL(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeDEL;
begin
  BQ := TIpHtmlNodeDEL.Create(Parent);
  BQ.ParseBaseProps(Self);
  BQ.Cite := FindAttribute(htmlAttrCITE);
  BQ.Datetime := FindAttribute(htmlAttrDATETIME);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagDELend]);
  EnsureClosure(IpHtmlTagDELend, EndTokens);
end;

procedure TIpHtml.ParseFontStyle(Parent : TIpHtmlNode;
  StartToken : TIpHtmlToken; const EndTokens : TIpHtmlTokenSet);
var
  CurStyle : TIpHtmlNodeFontStyle;
begin
  CurStyle := TIpHtmlNodeFontStyle.Create(Parent);
  case StartToken of
  IpHtmlTagTT :
    CurStyle.Style := hfsTT;
  IpHtmlTagI :
    CurStyle.Style := hfsI;
  IpHtmlTagB :
    CurStyle.Style := hfsB;
  IpHtmlTagU :
    CurStyle.Style := hfsU;
  IpHtmlTagSTRIKE :
    CurStyle.Style := hfsSTRIKE;
  IpHtmlTagS :
    CurStyle.Style := hfsS;
  IpHtmlTagBIG :
    CurStyle.Style := hfsBIG;
  IpHtmlTagSMALL :
    CurStyle.Style := hfsSMALL;
  IpHtmlTagSUB :
    CurStyle.Style := hfsSUB;
  IpHtmlTagSUP :
    CurStyle.Style := hfsSUP;
  end;
  CurStyle.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(CurStyle, EndTokens);
  EnsureClosure(succ(StartToken), EndTokens);
end;

procedure TIpHtml.ParseHR(Parent : TIpHtmlNode);
var
  NewRule : TIpHtmlNodeHR;
begin
  NewRule := TIpHtmlNodeHR.Create(Parent);
  with NewRule do begin
    Align := ParseImageAlignment(hiaCenter);
    NoShade := ParseBoolean(htmlAttrNOSHADE);
    Size := ParseHtmlInteger2(htmlAttrSIZE, 1);
    Size.OnChange := WidthChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '100%');
    Width.OnChange := WidthChanged;
    Color := ColorFromString(FindAttribute(htmlAttrCOLOR));
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseBR(Parent : TIpHtmlNode);
var
  BR : TIpHtmlNodeBR;
begin
  BR := TIpHtmlNodeBR.Create(Parent);
  BR.Clear := ParseBRClear;
  BR.Id := FindAttribute(htmlAttrID);
  BR.ClassId :=FindAttribute(htmlAttrCLASS);
  BR.Title :=FindAttribute(htmlAttrTITLE);
  BR.Style :=FindAttribute(htmlAttrSTYLE);
  NextToken;
end;

procedure TIpHtml.ParseNOBR(Parent : TIpHtmlNode);
begin
  NextToken;
  ParseBodyText(TIpHtmlNodeNOBR.Create(Parent), [IpHtmlTagNOBRend]);
  if CurToken = IpHtmlTagNOBRend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOBRend);
end;


procedure TIpHtml.ParsePhraseElement(Parent : TIpHtmlNode;
  StartToken, EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  CurPhrase : TIpHtmlNodePhrase;
begin
  CurPhrase := TIpHtmlNodePhrase.Create(Parent);
  case StartToken of
  IpHtmlTagEM :
    CurPhrase.Style := hpsEM;
  IpHtmlTagSTRONG :
    CurPhrase.Style := hpsSTRONG;
  IpHtmlTagDFN :
    CurPhrase.Style := hpsDFN;
  IpHtmlTagCODE :
    CurPhrase.Style := hpsCODE;
  IpHtmlTagSAMP :
    CurPhrase.Style := hpsSAMP;
  IpHtmlTagKBD :
    CurPhrase.Style := hpsKBD;
  IpHtmlTagVAR :
    CurPhrase.Style := hpsVAR;
  IpHtmlTagCITE :
    CurPhrase.Style := hpsCITE;
  IpHtmlTagABBR :
    CurPhrase.Style := hpsABBR;
  IpHtmlTagACRONYM :
    CurPhrase.Style := hpsACRONYM;
  end;
  CurPhrase.ParseBaseProps(Self);
  NextToken; // this can not be before previous line, as NextToken resets properties
  ParseBodyText(CurPhrase, [EndToken] + EndTokens);
  if CurToken = EndToken then
    NextToken
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseAnchor(Parent : TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
var
  CurAnchor : TIpHtmlNodeA;
begin
  CurAnchor := TIpHtmlNodeA.Create(Parent);
  {$IFDEF IP_LAZARUS}
  FTabList.Add(CurAnchor);
  {$ENDIF}
  with CurAnchor do begin
    Name := FindAttribute(htmlAttrNAME);
    HRef := FindAttribute(htmlAttrHREF);
    Rel := FindAttribute(htmlAttrREL);
    Rev := FindAttribute(htmlAttrREV);
    Title := FindAttribute(htmlAttrTITLE);
    ParseBaseProps(Self);
    Shape := ParseShape;
    TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
    Target := FindAttribute(htmlAttrTARGET);
  end;
  NextToken;
  ParseBodyText(CurAnchor, EndTokens + [IpHtmlTagAend] - [IpHtmlTagA]);
  if CurToken = IpHtmlTagAend then
    NextToken
  else
  if CurToken = IpHtmlTagA then
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagAend);
  if (CurAnchor.ChildCount = 0)
  and (CurAnchor.Name <> '') then
    TIpHtmlNodeText.Create(CurAnchor).FEscapedText := '&xxxxxx;';
end;

procedure TIpHtml.ParseIMG(Parent : TIpHtmlNode);
var
  CurIMG : TIpHtmlNodeIMG;
begin
  CurIMG := TIpHtmlNodeIMG.Create(Parent);
  with CurIMG do begin
    Src := FindAttribute(htmlAttrSRC);
    Alt := FindAttribute(htmlAttrALT);
    Align := ParseImageAlignment(hiaBottom);
    Height := ParsePixels(htmlAttrHEIGHT, '');
      {ParseInteger(htmlAttrHEIGHT, -1);}
    Height.OnChange := DimChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := DimChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    HSpace := ParseInteger(htmlAttrHSPACE, 0);
    VSpace := ParseInteger(htmlAttrVSPACE, 0);
    UseMap := FindAttribute(htmlAttrUSEMAP);
    IsMap := ParseBoolean(htmlAttrISMAP);
    ParseBaseProps(Self);
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
  end;
  NextToken;
end;

procedure TIpHtml.ParseApplet(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
var
  CurApplet : TIpHtmlNodeAPPLET;
  CurParam : TIpHtmlNodePARAM;
begin
  CurApplet := TIpHtmlNodeAPPLET.Create(Parent);
  with CurApplet do begin
    Codebase := FindAttribute(htmlAttrCODEBASE);
    Code := FindAttribute(htmlAttrCODE);
    Alt := FindAttribute(htmlAttrALT);
    Name := FindAttribute(htmlAttrNAME);
    Height := ParseInteger(htmlAttrHEIGHT, -1);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := WidthChanged;
    Align := ParseImageAlignment(hiaBottom);
    HSpace := ParseInteger(htmlAttrHSPACE, 1);
    VSpace := ParseInteger(htmlAttrVSPACE, 1);
    Archive := FindAttribute(htmlAttrARCHIVE);
    ObjectCode := FindAttribute(htmlAttrOBJECT);
    Id := FindAttribute(htmlAttrID);
    ClassID := FindAttribute(htmlAttrCLASS);
    Title := FindAttribute(htmlAttrTITLE);
    Style := FindAttribute(htmlAttrSTYLE);
  end;
  NextToken;
  while not (CurToken in EndTokens + [IpHtmlTagAPPLETend]) do begin
    case CurToken of
    IpHtmlTagPARAM :
      begin
        CurParam := TIpHtmlNodePARAM.Create(CurApplet);
        with CurParam do begin
          {CurParam := TIpHtmlNodePARAM.Create(CurApplet);}
          Name := FindAttribute(htmlAttrNAME);
          Value := FindAttribute(htmlAttrVALUE);
          Id := FindAttribute(htmlAttrID);
          ValueType := ParseObjectValueType;
        end;
        NextToken;
      end;
    else
      ParseText([IpHtmlTagAPPLETend, IpHtmlTagPARAM], CurApplet);
    end;
  end;
  EnsureClosure(IpHtmlTagAPPLETend, EndTokens);
end;

procedure TIpHtml.ParseOBJECT(Parent : TIpHtmlNode);
var
  CurOBJECT : TIpHtmlNodeOBJECT;
  CurParam : TIpHtmlNodePARAM;
begin
  CurOBJECT := TIpHtmlNodeOBJECT.Create(Parent);
  with CurOBJECT do begin
    ClassID := FindAttribute(htmlAttrCLASSID);
    Codebase := FindAttribute(htmlAttrCODEBASE);
    Data := FindAttribute(htmlAttrDATA);
    CodeType := FindAttribute(htmlAttrCODETYPE);
    Archive := FindAttribute(htmlAttrARCHIVE);
    Standby := FindAttribute(htmlAttrSTANDBY);
    Align := ParseImageAlignment(hiaBottom);
    Height := ParseInteger(htmlAttrHEIGHT, -1);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := WidthChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    HSpace := ParseInteger(htmlAttrHSPACE, 1);
    VSpace := ParseInteger(htmlAttrVSPACE, 1);
    UseMap := FindAttribute(htmlAttrUSEMAP);
    Declare := ParseBoolean(htmlAttrDECLARE);
    ParseBaseProps(Self);
    Name := FindAttribute(htmlAttrNAME);
  end;
  NextToken;
  while not (CurToken = IpHtmlTagOBJECTend) do begin
    case CurToken of
    IpHtmlTagPARAM :
      begin
        CurParam := TIpHtmlNodePARAM.Create(CurObject);
        with CurParam do begin
          Name := FindAttribute(htmlAttrNAME);
          Value := FindAttribute(htmlAttrVALUE);
          Id := FindAttribute(htmlAttrID);
          ValueType := ParseObjectValueType;
        end;
        NextToken;
      end;
    else
      ParseText([IpHtmlTagOBJECTend, IpHtmlTagPARAM], CurObject);
    end;
  end;
  if CurToken = IpHtmlTagOBJECTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagOBJECTend);
end;

procedure TIpHtml.ParseTableRow(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
var
  CurHeader : TIpHtmlNodeTH;
  CurTableCell : TIpHtmlNodeTD;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagTH :
      begin
        CurHeader := TIpHtmlNodeTH.Create(Parent);
        with CurHeader do begin
          Nowrap := ParseBoolean(htmlAttrNOWRAP);
          Rowspan := ParseInteger(htmlAttrROWSPAN, 1);
          Colspan := ParseInteger(htmlAttrCOLSPAN, 1);
          ParseBaseProps(Self);
          Align := ParseCellAlign(haCenter{haDefault});
          VAlign := ParseVAlignment3;
          Width := ParseHyperLength(htmlAttrWIDTH, '');
          Width.OnChange := DimChanged;
          Height := ParsePixels(htmlAttrHEIGHT, '');
            {ParseInteger(htmlAttrHEIGHT, -1);}
          Height.OnChange := DimChanged;
          BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
        end;
        NextToken;
        ParseBodyText(CurHeader,
                      EndTokens + [IpHtmlTagTH, IpHtmlTagTHend, IpHtmlTagTD]);
        if CurToken in [IpHtmlTagTHend, IpHtmlTagTDend] then
          NextRealToken;
      end;
    IpHtmlTagTD :
      begin
        CurTableCell := TIpHtmlNodeTD.Create(Parent);
        with CurTableCell do begin
          Nowrap := ParseBoolean(htmlAttrNOWRAP);
          Rowspan := ParseInteger(htmlAttrROWSPAN, 1);
          Colspan := ParseInteger(htmlAttrCOLSPAN, 1);
          ParseBaseProps(Self);
          Align := ParseCellAlign(haDefault);
          VAlign := ParseVAlignment3;
          Width := ParseHyperLength(htmlAttrWIDTH, '');
          Width.OnChange := DimChanged;
          Height := ParsePixels(htmlAttrHEIGHT, '');
            {ParseInteger(htmlAttrHEIGHT, -1);}
          Height.OnChange := DimChanged;
          BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
        end;
        NextToken;
        ParseBodyText(CurTableCell, EndTokens + [IpHtmlTagTD, IpHtmlTagTDend]);
        if CurToken = IpHtmlTagTDend then
          NextRealToken;
      end;
    else
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseTableRows(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);

  procedure FixupPercentages(CurRow: TIpHtmlNodeTR);
  var
    i, Pt, P0: Integer;
  begin
    Pt := 0;
    P0 := 0;
    for i := 0 to CurRow.ChildCount - 1 do
      if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
        case TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthType of
        hlUndefined :
          Inc(P0);
        hlPercent :
          Inc(Pt, TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthValue);
        end;
    if (Pt > 0) and (Pt < 100) and (P0 > 0) then begin
      Pt := (100 - Pt) div P0;
      for i := 0 to CurRow.ChildCount - 1 do
        if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
          with TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width do
            if LengthType = hlUndefined then begin
              LengthType := hlPercent;
              LengthValue := Pt;
            end;
    end;
  end;

var
  CurRow : TIpHtmlNodeTR;
begin
  CurRow := nil;
  while not (CurToken in EndTokens) do
    case CurToken of
      IpHtmlTagTR :
        begin
          if CurRow <> nil then
            FixupPercentages(CurRow);
          CurRow := TIpHtmlNodeTR.Create(Parent);
          CurRow.ParseBaseProps(Self);
          CurRow.Align := ParseAlignment;
          CurRow.VAlign := ParseVAlignment;
          CurRow.LoadAndApplyCSSProps;
          NextRealToken;
          ParseTableRow(CurRow,
                        EndTokens + [IpHtmlTagTRend, IpHtmlTagTR] -
                                    [IpHtmlTagTH, IpHtmlTagTD]);
          while CurToken = IpHtmlTagTRend do
            NextToken;
        end;
      IpHtmlTagTH,
      IpHtmlTagTD :
        begin
          if CurRow <> nil then
            FixupPercentages(CurRow);
          CurRow := TIpHtmlNodeTR.Create(Parent);
          ParseTableRow(CurRow,
                        EndTokens + [IpHtmlTagTR] - [IpHtmlTagTH, IpHtmlTagTD]);
        end;
      else
        NextToken;
    end;
  if CurRow <> nil then
    FixupPercentages(CurRow);
end;

procedure TIpHtml.ParseTableBody(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurHead : TIpHtmlNodeTHEAD;
  CurFoot : TIpHtmlNodeTFOOT;
  CurBody : TIpHtmlNodeTBODY;
begin
  if CurToken = IpHtmlTagTHEAD then begin
    CurHead := TIpHtmlNodeTHEAD.Create(Parent);
    CurHead.ParseBaseProps(Self);
    CurHead.Align := ParseCellAlign(haLeft);
    CurHead.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(CurHead,
                   EndTokens + [IpHtmlTagTFOOT, IpHtmlTagTBODY, IpHtmlTagTHEADend] -
                               [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
    if CurToken = IpHtmlTagTHEADend then
      NextToken;
  end;
  if CurToken = IpHtmlTagTFOOT then begin
    CurFoot := TIpHtmlNodeTFOOT.Create(Parent);
    CurFoot.ParseBaseProps(Self);
    CurFoot.Align := ParseCellAlign(haLeft);
    CurFoot.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(CurFoot,
                   EndTokens + [IpHtmlTagTBODY, IpHtmlTagTFOOTend] -
                               [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
    if CurToken = IpHtmlTagTFOOTend then
      NextToken;
  end;
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagTBODY :
      begin
        CurBody := TIpHtmlNodeTBODY.Create(Parent);
        CurBody.ParseBaseProps(Self);
        CurBody.Align := ParseCellAlign(haLeft);
        CurBody.VAlign := ParseVAlignment3;
        NextToken;
        ParseTableRows(CurBody,
                       EndTokens + [IpHtmlTagTBODYend] -
                         [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD, IpHtmlTagTRend]);
        if CurToken = IpHtmlTagTBODYend then
          NextToken;
      end;
    IpHtmlTagTR :
      begin
        CurBody := TIpHtmlNodeTBODY.Create(Parent);
        ParseTableRows(CurBody,
                       EndTokens - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
      end;
    else
      Exit;
    end;
  end;
end;

procedure TIpHtml.ParseColGroup(Parent: TIpHtmlNode);
var
  CurColGroup : TIpHtmlNodeCOLGROUP;
  CurCol : TIpHtmlNodeCOL;
begin
  while CurToken = IpHtmlTagCOLGROUP do begin
    CurColGroup := TIpHtmlNodeCOLGROUP.Create(Parent);
    with CurColGroup do begin
      ParseBaseProps(Self);
      Span := ParseInteger(htmlAttrSPAN, 1);
      Width := ParseHyperMultiLength(htmlAttrWIDTH, '');
    end;
    NextToken;
    SkipTextTokens;
    while CurToken = IpHtmlTagCOL do begin
      CurCol := TIpHtmlNodeCOL.Create(CurColGroup);
      with CurCol do begin
        ParseBaseProps(Self);
        Span := ParseInteger(htmlAttrSPAN, 1);
        Width := ParseHyperMultiLength(htmlAttrWIDTH, '');
      end;
      NextToken;
      SkipTextTokens;
    end;
    if CurToken = IpHtmlTagCOLGROUPend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseTABLE(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurTable : TIpHtmlNodeTABLE;
  CurCaption : TIpHtmlNodeCAPTION;
begin
  CurTable := TIpHtmlNodeTABLE.Create(Parent);
  with CurTable do begin
    Align := ParseImageAlignment(hiaBottom);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := WidthChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    CellSpacing := ParseInteger(htmlAttrCELLSPACING, 2);
    CellPadding := ParseInteger(htmlAttrCELLPADDING, 2);
    ParseBaseProps(Self);
    Summary := FindAttribute(htmlAttrSUMMARY);
    Frame := ParseFrameProp(Frame);
    Rules := ParseRules(Rules);
    BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
  end;

  repeat
    NextToken;
  until CurToken in
     [IpHtmlTagCAPTION, IpHtmlTagCOLGROUP, IpHtmlTagTHEAD, IpHtmlTagTFOOT,
      IpHtmlTagTBODY, IpHtmlTagTR, IpHtmlTagTABLEend, IpHtmlTagEOF];

  if CurToken = IpHtmlTagCAPTION then begin
    CurCaption := TIpHtmlNodeCAPTION.Create(CurTable);
    CurCaption.Align := ParseVAlignment2;
    CurCaption.ParseBaseProps(Self);
    ParseBodyText(CurCaption,
                  [IpHtmlTagCAPTIONend, IpHtmlTagTABLEend, IpHtmlTagTBODY]);
    if CurToken in EndTokens then
    else
    if CurToken = IpHtmlTagCAPTIONend then
      NextToken
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagCAPTIONend)
      else begin
        while not (CurToken in EndTokens + [IpHtmlTagCAPTIONend]) do
          NextToken;
        if CurToken = IpHtmlTagCAPTIONend then
          NextToken;
      end;
    CurTable.FCaption := CurCaption;
  end;
  ParseColgroup(CurTable);
  SkipTextTokens;
  ParseTableBody(CurTable, EndTokens + [IpHtmlTagTABLEend]
    - [IpHtmlTagTR, IpHtmlTagP, IpHtmlTagPend, IpHTMLTagCENTERend,
       IpHtmlTagLEFTend, IpHtmlTagRIGHTend, IpHtmlTagBLINKend, IpHtmlTagBLOCKQUOTEend
      ]);
  SkipTextTokens;
  EnsureClosure(IpHtmlTagTABLEend, EndTokens);
end;

procedure TIpHtml.ParseMAP(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
var
  CurMap : TIpHtmlNodeMAP;
begin
  CurMap := TIpHtmlNodeMAP.Create(Parent);
  CurMap.Name := FindAttribute(htmlAttrNAME);
  CurMap.ParseBaseProps(Self);
  NextToken;
  while not (CurToken in EndTokens + [IpHtmlTagMAPend]) do begin
    case CurToken of
    IpHtmlTagAREA :
      begin
        with TIpHtmlNodeAREA.Create(CurMap) do begin
          Shape := ParseShape;
          Coords := FindAttribute(htmlAttrCOORDS);
          HRef := FindAttribute(htmlAttrHREF);
          NoHRef := ParseBoolean(htmlAttrNOHREF);
          Alt := FindAttribute(htmlAttrALT);
          TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
          Target := FindAttribute(htmlAttrTARGET);
          ParseBaseProps(Self);
        end;
        NextToken;
      end;
    else
      if FlagErrors then
        ReportExpectedError('</MAP> or <AREA>')
      else
        NextToken;
    end;
  end;
  EnsureClosure(IpHtmlTagMAPend, EndTokens);
end;

procedure TIpHtml.ParseBasefont(Parent : TIpHtmlNode);
var
  CurBasefont : TIpHtmlNodeBASEFONT;
begin
  CurBasefont := TIpHtmlNodeBASEFONT.Create(Parent);
  if CurBasefont=nil then ;
  CurBasefont.Size := ParseInteger(htmlAttrSIZE, 3);
  NextToken;
end;

procedure TIpHtml.ParseInline(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  case CurToken of
  IpHtmlTagP : ParseParagraph(Parent, EndTokens); {moved from block}
  IpHtmlTagFont : ParseFont(Parent, EndTokens);
  IpHtmlTagDIV : ParseDiv(Parent, EndTokens);
  IpHtmlTagSPAN : ParseSpan(Parent, EndTokens);
  IpHtmlTagLEFT : ParseLeft(Parent, EndTokens);
  IpHtmlTagCENTER : ParseCenter(Parent, EndTokens);
  IpHtmlTagRIGHT : ParseRight(Parent, EndTokens);
  IpHtmlTagBLINK : ParseBlink(Parent, EndTokens);
  IpHtmlTagQ : ParseQ(Parent, EndTokens);
  IpHtmlTagHR : ParseHR(Parent);
  IpHtmlTagTT, IpHtmlTagI, IpHtmlTagB, IpHtmlTagU, IpHtmlTagSTRIKE, IpHtmlTagS,
  IpHtmlTagBIG, IpHtmlTagSMALL, IpHtmlTagSUB, IpHtmlTagSUP :
    ParseFontStyle(Parent, CurToken, EndTokens + [succ(CurToken)]);
  IpHtmlTagEM, IpHtmlTagSTRONG, IpHtmlTagDFN, IpHtmlTagCODE,
  IpHtmlTagSAMP, IpHtmlTagKBD, IpHtmlTagVAR, IpHtmlTagCITE,
  IpHtmlTagABBR, IpHtmlTagACRONYM :
    ParsePhraseElement(Parent, CurToken, succ(CurToken), EndTokens);
  IpHtmlTagA : ParseAnchor(Parent, EndTokens);
  IpHtmlTagBASEFONT : ParseBasefont(Parent);
  IpHtmlTagBR : ParseBR(Parent);
  IpHtmlTagNOBR : ParseNOBR(Parent);
  IpHtmlTagMAP :
    ParseMAP(Parent, EndTokens);
  IpHtmlTagText :
    begin
      TIpHtmlNodeText.Create(Parent).FEscapedText := GetTokenString;
      NextToken;
    end;
  IpHtmlTagINPUT,
  IpHtmlTagSELECT,
  IpHtmlTagButton,
  IpHtmlTagTEXTAREA :
    ParseFormFields(Parent, EndTokens);
  IpHtmlTagINS :
    ParseIns(Parent, EndTokens);
  IpHtmlTagDEL :
    ParseDel(Parent, EndTokens);
  IpHtmlTagIFRAME :
    ParseIFRAME(Parent);
  IpHtmlTagSCRIPT :
    ParseScript(Parent, EndTokens);
  IpHtmlTagNOSCRIPT :
    ParseNoscript(Parent);
  IpHtmlTagSTYLE :
    ParseStyle(Parent);
  else
    NextToken;
  end;
end;

procedure TIpHtml.ParseBlock(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  case CurToken of
  IpHtmlTagH1 : ParseHeader(Parent, IpHtmlTagH1end, 1);
  IpHtmlTagH2 : ParseHeader(Parent, IpHtmlTagH2end, 2);
  IpHtmlTagH3 : ParseHeader(Parent, IpHtmlTagH3end, 3);
  IpHtmlTagH4 : ParseHeader(Parent, IpHtmlTagH4end, 4);
  IpHtmlTagH5 : ParseHeader(Parent, IpHtmlTagH5end, 5);
  IpHtmlTagH6 : ParseHeader(Parent, IpHtmlTagH6end, 6);
  {IpHtmlTagP : ParseParagraph(Parent, EndTokens);} {moved to inline}
  IpHtmlTagDIR : ParseUnorderedList(Parent, IpHtmlTagDIRend, EndTokens);
  IpHtmlTagMENU : ParseUnorderedList(Parent, IpHtmlTagMENUend, EndTokens);
  IpHtmlTagUL : ParseUnorderedList(Parent, IpHtmlTagULend, EndTokens);
  IpHtmlTagDL : ParseDefinitionList(Parent, EndTokens);
  IpHtmlTagOL : ParseOrderedList(Parent, EndTokens);
  IpHtmlTagPRE : ParsePre(Parent, EndTokens);
  IpHtmlTagBLOCKQUOTE : ParseBlockQuote(Parent, EndTokens);
  IpHtmlTagFORM : ParseForm(Parent, EndTokens);
  IpHtmlTagTABLE : ParseTable(Parent, EndTokens);
  IpHtmlTagIMG : ParseIMG(Parent);
  IpHtmlTagOBJECT : ParseObject(Parent);
  IpHtmlTagAPPLET : ParseApplet(Parent, EndTokens);
  IpHtmlTagADDRESS : ParseAddress(Parent);
  IpHtmlTagEof : Exit;
  IpHtmlTagFRAMESET : ParseFrameSet(Parent, EndTokens + [IpHtmlTagFRAMESETend]);
  IpHtmlTagUnknown :
    if FlagErrors then
      ReportError(SHtmlUnknownTok)
    else
      NextToken;
  end;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtml.ParseStyleSheet(Parent: TIpHtmlNode; HRef: String);
var
  StyleStream: TStream;
begin
  //debugln(['TIpHtml.ParseStyleSheet ',href,' ',Parent is TIpHtmlNodeHEAD,' ',DbgSName(FDataProvider)]);
  StyleStream:=nil;
  
  if Parent is TIpHtmlNodeHEAD then begin
    if FDataProvider<>nil then begin
      Href := FDataProvider.BuildURL(FCurURL, HRef);
      StyleStream := FDataProvider.DoGetStream(HRef);
    end;
  end else
  if Parent is TIpHtmlNodeSTYLE then
    StyleStream := TStringStream.Create(Href);
    
  if StyleStream<>nil then
    with TCSSReader.Create(StyleStream, FCSS) do begin
      ParseCSS;
      Free;
      StyleStream.Free;
    end;
end;
{$ENDIF}


procedure TIpHtml.ParseBodyText(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  Inc(InBlock);
  try
    while not (CurToken in EndTokens) do begin
      case CurToken of
      IpHtmlTagH1,
      IpHtmlTagH2,
      IpHtmlTagH3,
      IpHtmlTagH4,
      IpHtmlTagH5,
      IpHtmlTagH6,
      {IpHtmlTagP,}
      IpHtmlTagDIR,
      IpHtmlTagMENU,
      IpHtmlTagUL,
      IpHtmlTagDL,
      IpHtmlTagOL,
      IpHtmlTagPRE,
      IpHtmlTagBLOCKQUOTE,
      IpHtmlTagFORM,
      IpHtmlTagTABLE,
      IpHtmlTagIMG,
      IpHtmlTagOBJECT,
      IpHtmlTagAPPLET,
      IpHtmlTagADDRESS,
      IpHtmlTagFRAMESET :
        ParseBlock(Parent, EndTokens);
      IpHtmlTagBODY :
        begin
          if Body = nil then begin
            TIpHtmlNodeBODY.Create(Parent);
            NextToken;
            ParseBodyText(Body, EndTokens);
          end
          else
            ParseInline(Parent, EndTokens);
        end;
      IpHtmlTagEof :
        Exit;
      else
        ParseInline(Parent, EndTokens);
      end;
    end;
  finally
    Dec(InBlock);
  end;
end;

function TIpHtml.FindAttribute(const AttrNameSet : TIpHtmlAttributesSet) : string;
begin
  Result := ParmValueArray[AttrNameSet];
end;

function TIpHtml.ParseInteger(const AttrNameSet: TIpHtmlAttributesSet; aDefault : Integer): Integer;
var
  S : string;
  Err : Integer;
  AttrName: string;
begin
  AttrName := TIpHtmlAttributesNames[AttrNameSet];
  S := FindAttribute(AttrNameSet);
  if length(S) = 0 then
    Result := aDefault
  else
  if CompareText(S, AttrName) = 0 then
    Result := 1
  else begin
    Val(S, Result, Err);
    if Err <> 0 then begin
      Result := aDefault;
      if FlagErrors then
        ReportError(SHtmlInvInt)
    end;
  end;
end;

function TIpHtml.ParseHtmlInteger2(const AttrNameSet: TIpHtmlAttributesSet;
         aDefault : Integer) : TIpHtmlInteger;
begin
  Result := TIpHtmlInteger.Create(ParseInteger(AttrNameSet, aDefault));
end;

function TIpHtml.ParseRelSize{(const Default : string)} : TIpHtmlRelSize;
var
  S : string;
  Err : Integer;
begin
  Result := TIpHtmlRelSize.Create;
  Result.FSizeType := hrsUnspecified;
  S := FindAttribute(htmlAttrSIZE);
  if length(S) = 0 then
    Exit; {S := Default;}
  Result.Value := 0;
  if (length(S) > 1) and (S[1] = '+') then begin
    Result.SizeType := hrsRelative;
    Delete(S,1,1);
  end else
  if (length(S) > 1) and (S[1] = '-') then begin
    Result.SizeType := hrsRelative;
  end else
    Result.SizeType := hrsAbsolute;
  Val(S, Result.FValue, Err);
  if Err <> 0 then
    if FlagErrors then
      ReportError(SHtmlInvInt);
end;

function TIpHtml.ParsePixels(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlPixels;
var
  S : string;
  Err : Integer;
begin
  Result := TIpHtmlPixels.Create;
  S := FindAttribute(AttrNameSet);
  if (S = '') then
    S := aDefault;
  if S = '' then
    Result.PixelsType := hpUndefined
  else begin
    Result.PixelsType := hpAbsolute;
    val(S, Result.FValue, Err);
    if (Err <> 0) or (Result.FValue < 0) then begin
      if FlagErrors then
        ReportError(SHtmlInvInt)
      else
        Result.FValue := 0;
    end;
  end;
end;

function TIpHtml.ParseHyperLength(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlLength;
var
  S : string;
  P, Err : Integer;
begin
  Result := TIpHtmlLength.Create;
  Result.LengthType := hlUndefined;
  S := FindAttribute(AttrNameSet);
  if length(S) = 0 then
    if length(aDefault) = 0 then exit
    else S := aDefault;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hlPercent;
    Delete(S, P, 1);
  end else
    Result.LengthType := hlAbsolute;
  val(S, Result.FLengthValue, Err);
  if (Err <> 0) or (Result.LengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hlUndefined;
  end else
    if (Result.LengthType = hlPercent)
    and (Result.LengthValue > 100) then
      Result.LengthValue := 100;
end;

function TIpHtml.ParseHyperMultiLength(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlMultiLength;
var
  S : string;
  P, Err : Integer;
begin
  Result := TIpHtmlMultiLength.Create;
  Result.LengthType := hmlUndefined;
  S := FindAttribute(AttrNameSet);
  if length(S) = 0 then
    if length(aDefault) = 0 then exit
    else S := aDefault;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hmlPercent;
    Delete(S, P, 1);
  end else begin
    P := CharPos('*', S);
    if P <> 0 then begin
      Result.LengthType := hmlRelative;
      Delete(S, P, 1);
    end else
      Result.LengthType := hmlAbsolute;
  end;
  val(S, Result.FLengthValue, Err);
  if (Err <> 0) or (Result.FLengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hmlUndefined;
  end;
end;

function TIpHtml.ParseHyperMultiLengthList(const AttrNameSet: TIpHtmlAttributesSet;
      const aDefault: string): TIpHtmlMultiLengthList;
var
  S, S2 : string;
  B, E, P, Err : Integer;
  NewEntry: TIpHtmlMultiLength;
begin
  {List.Entries := 0;}
  Result := TIpHtmlMultiLengthList.Create;
  S := FindAttribute(AttrNameSet);
  if length(S) = 0 then
    if length(aDefault) = 0 then exit
    else S := aDefault;
  B := 1;
  while B <= length(S) do begin
    E := B;
    repeat
      Inc(E);
    until (E > length(S)) or (S[E] = ',');
    S2 := copy(S, B, E - B);
    NewEntry := TIpHtmlMultiLength.Create;
    {List.Values[List.Entries].LengthType := hmlUndefined;}
    NewEntry.LengthType := hmlUndefined;
    P := CharPos('%', S2);
    if P <> 0 then begin
      {List.Values[List.Entries].LengthType := hmlPercent;}
      NewEntry.LengthType := hmlPercent;
      Delete(S2, P, 1);
    end else begin
      P := CharPos('*', S2);
      if P <> 0 then begin
        {List.Values[List.Entries].LengthType := hmlRelative;}
        NewEntry.LengthType := hmlRelative;
        Delete(S2, P, 1);
      end else
        {List.Values[List.Entries].LengthType := hmlAbsolute;}
        NewEntry.LengthType := hmlAbsolute;
    end;
    if S2 = '' then
      {List.Values[List.Entries].LengthValue := 0}
      NewEntry.LengthValue := 0
    else begin
      {val(S2, List.Values[List.Entries].FLengthValue, Err);}
      val(S2, NewEntry.FLengthValue, Err);
      {if (Err <> 0) or (List.Values[List.Entries].LengthValue < 0) then begin}
      if (Err <> 0) or (NewEntry.FLengthValue < 0) then begin
        if FlagErrors then
          ReportError(SHtmlInvInt)
        else
          {List.Values[List.Entries].LengthType := hmlUndefined;}
          NewEntry.LengthType := hmlUndefined;
      end;
    end;
    {Inc(List.Entries);}
    Result.AddEntry(NewEntry);
    B := E + 1;
  end;
end;

function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr;
var
  OrgAvail, i, S : Integer;
begin
  Result := TIntArr.Create;
  if List.Entries = 0 then begin
    Sections := 1;
    Result[0] := Avail;
    Exit;
  end;
  OrgAvail := Avail;
  Sections := List.Entries;
  for i := 0 to Pred(List.Entries) do begin
    if List.Values[i].LengthType = hmlAbsolute then begin
      if Avail >= List.Values[i].LengthValue then begin
        Result[i] := List.Values[i].LengthValue;
        Dec(Avail, Result[i]);
      end else begin
        Result[i] := Avail;
        Avail := 0;
      end;
    end else
      Result[i] := 0;
  end;
  if Avail > 0 then begin
    for i := 0 to Pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        Result[i] := round(List.Values[i].LengthValue * Avail / 100);
    for i := 0 to Pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        Dec(Avail, Result[i]);
    if Avail > 0 then begin
      S := 0;
      for i := 0 to Pred(List.Entries) do
        if (List.Values[i].LengthType = hmlRelative) then
          Inc(S, List.Values[i].LengthValue);
      if S > 0 then
        for i := 0 to Pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative) then begin
            Result[i] := round(List.Values[i].LengthValue * Avail / S);
            Dec(Avail, Result[i]);
          end;
      if Avail > 0 then
        for i := 0 to Pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative)
          and (List.Values[i].LengthValue = 0) then begin
            Result[i] := Avail;
            break;
          end;
    end;
  end;
  repeat
    S := 0;
    for i := 0 to Pred(List.Entries) do
      Inc(S, Result[i]);
    S := OrgAvail - S;
    if S > 0 then
      for i := 0 to Pred(List.Entries) do begin
        {Inc(Result[i]);}
        Result[i] := Result[i] + 1;
        Dec(S);
        if S = 0 then break;
      end;
    if S < 0 then
      for i := 0 to Pred(List.Entries) do begin
        {Dec(Result[i]);}
        Result[i] := Result[i] - 1;
        Inc(S);
        if S = 0 then break;
      end;
  until S = 0;
end;

function TIpHtml.ParseBoolean(const AttrNameSet: TIpHtmlAttributesSet): Boolean;
begin
  Result := length(ParmValueArray[AttrNameSet]) > 0;
end;

const
  TIpHtmlOLStyleNames : array[TIpHtmlOLStyle] of char = (
   '1', 'a', 'A', 'i', 'I');

function TIpHtml.ParseOLStyle(Default : TIpHtmlOLStyle) : TIpHtmlOLStyle;
var
  S : string;
begin
  Result := Default;
  S := FindAttribute(htmlAttrTYPE);
  if length(S) > 0 then
  begin
    for result:= low(TIpHtmlOLStyle) to high(TIpHtmlOLStyle) do
      if S = TIpHtmlOLStyleNames[result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

function TIpHtml.ParseULStyle(Default : TIpHtmlULType) : TIpHtmlULType;
var
  S : string;
begin
  Result := Default;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if length(S) = 0 then exit;
  case S[1] of
    'C': if S = 'CIRCLE' then Result := ulCircle;
    'D': if S = 'DISC' then Result := ulDisc;
    'S': if S = 'SQUARE' then Result := ulSquare;
    else
      if FlagErrors then
        ReportError(SHtmlInvType);
  end;
end;

function TIpHtml.ParseAlignment : TIpHtmlAlign;
begin
  Result := GetAlignmentForStr(FindAttribute(htmlAttrALIGN), haLeft);
//  if FlagErrors then
//    ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseVAlignment : TIpHtmlVAlign;
var
  S : string;
begin
  Result := hvaMiddle;
  S := UpperCase(FindAttribute(htmlAttrVALIGN));
  if length(S) = 0 then exit;
  case S[1] of
    'B': if S = 'BOTTOM' then Result := hvaBottom;
    'C','M': if (S = 'MIDDLE') or (S = 'CENTER') then exit;
    'T': if S = 'TOP' then Result := hvaTop;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

function TIpHtml.ParseVAlignment2: TIpHtmlVAlignment2;
var
  S : string;
begin
  Result := hva2Top;
  S := UpperCase(FindAttribute(htmlAttrALIGN));
  if length(S) = 0 then exit;
  case S[1] of
    'B': if S = 'BOTTOM' then Result := hva2Bottom;
    'L': if S = 'LEFT' then Result := hva2Left;
    'R': if S = 'RIGHT' then Result := hva2Right;
    'T': if (S = 'TOP') then exit;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

const
  TIpHtmlImageAlignNames : array[TIpHtmlImageAlign] of string = (
    'TOP', 'MIDDLE', 'BOTTOM', 'LEFT', 'RIGHT', 'CENTER');

function TIpHtml.ParseImageAlignment(aDefault: TIpHtmlImageAlign) : TIpHtmlImageAlign;
var
  S : string;
begin
  Result := aDefault;
  S := UpperCase(FindAttribute(htmlAttrALIGN));
  if length(S) = 0 then exit;
  for result:=low(TIpHtmlImageAlign) to high(TIpHtmlImageAlign) do
    if S = TIpHtmlImageAlignNames[result] then exit;
  if FlagErrors then
    ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseObjectValueType: TIpHtmlObjectValueType;
var
  S : string;
begin
  Result := hovtData;
  S := UpperCase(FindAttribute(htmlAttrVALUETYPE));
  if length(S) = 0 then exit;
  case S[1] of
    'D': if S = 'DATA' then exit;
    'O': if S = 'OBJECT' then Result := hovtObject;
    'R': if S = 'REF' then Result := hovtRef;
    else
      if FlagErrors then
        ReportError(SHtmlInvValType);
  end;
end;

function TIpHtml.ParseShape : TIpHtmlMapShape;
var
  S : string;
begin
  Result := hmsDefault;
  S := UpperCase(FindAttribute(htmlAttrSHAPE));
  if length(S) = 0 then exit;
  case S[1] of
    'C': if S = 'CIRCLE' then Result := hmsCircle;
    'D': if S = 'DEFAULT' then exit;
    'P': if (S = 'POLY') or (S = 'POLYGON') then
           Result := hmsPoly;
    'R': if (S = 'RECT') then Result := hmsRect;
    else
      if FlagErrors then
        ReportError(SHtmlInvShape);
  end;
end;

function TIpHtml.ParseMethod : TIpHtmlFormMethod;
var
  S : string;
begin
  Result := hfmGet;
  S := UpperCase(FindAttribute(htmlAttrMETHOD));
  if (length(S) = 0) or (S = 'GET') then
  else
  if S = 'POST' then
    Result := hfmPost
  else
    if FlagErrors then
      ReportError(SHtmlInvMethod);
end;

function TIpHtml.ParseBRClear : TIpHtmlBreakClear;
var
  S : string;
begin
  Result := hbcNone;
  S := UpperCase(FindAttribute(htmlAttrCLEAR));
  if length(S) = 0 then exit;
  case S[1] of
    'A','C': if (S = 'ALL') or (S = 'CLEAR') then
               Result := hbcAll;
    'L': if S = 'LEFT' then Result := hbcLeft;
    'R': if S = 'RIGHT' then Result := hbcRight;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

function TIpHtml.ParseDir : TIpHtmlDirection;
var
  S : string;
begin
  Result := hdLTR;
  S := UpperCase(FindAttribute(htmlAttrDIR));
  if (length(S) = 0) or (S = 'LTR') then
  else
  if (S = 'RTL') then
    Result := hdRTL
  else
    if FlagErrors then
      ReportError(SHtmlInvDir);
end;

function TIpHtml.ColorFromString(S : string) : TColor;
var
  R, G, B, Err : Integer;
begin
  Result := -1;
  if S = '' then
    Exit;
  S := UpperCase(S);
  if S[1] = '#' then
    if length(S) <> 7 then
      if FlagErrors then
        ReportError(SHtmlInvColor + S)
      else
    else begin
      val('$'+Copy(S,2,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,4,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,6,2), B, Err);
      if Err <> 0 then
        B := 255;
      Result := RGB(R, G, B);
    end
  else
  if BinSearchNamedColor(S, result) then exit
  else
  if length(S) = 6 then
    try
      val('$'+Copy(S,1,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,3,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,5,2), B, Err);
      if Err <> 0 then
        B := 255;
      Result := RGB(R, G, B);
    except
      if FlagErrors then
        ReportError(SHtmlInvColor + S)
      else
        Result := -1;
    end;
end;

procedure TIpHtml.ParseFrame(Parent : TIpHtmlNode);
var
  CurFrame : TIpHtmlNodeFRAME;
begin
  CurFrame := TIpHtmlNodeFRAME.Create(Parent);
  with CurFrame do begin
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
    Src := FindAttribute(htmlAttrSRC);
    FrameBorder := ParseInteger(htmlAttrBORDER, 1);
    MarginWidth := ParseInteger(htmlAttrMARGINWIDTH, 1);
    MarginHeight := ParseInteger(htmlAttrMARGINHEIGHT, 1);
    NoResize := ParseBoolean(htmlAttrNORESIZE);
    Scrolling := ParseFrameScrollingProp;
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseIFrame(Parent : TIpHtmlNode);
var
  CurFrame : TIpHtmlNodeIFRAME;
begin
  CurFrame := TIpHtmlNodeIFRAME.Create(Parent);
  with CurFrame do begin
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
    Src := FindAttribute(htmlAttrSRC);
    FrameBorder := ParseInteger(htmlAttrBORDER, 1);
    MarginWidth := ParseInteger(htmlAttrMARGINWIDTH, 1);
    MarginHeight := ParseInteger(htmlAttrMARGINHEIGHT, 1);
    Scrolling := ParseFrameScrollingProp;
    Align := ParseAlignment;
    Height := ParseHyperLength(htmlAttrHEIGHT, '');
    Height.OnChange := WidthChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := WidthChanged;
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurFrame, [IpHtmlTagIFRAMEend]);
  if CurToken = IpHtmlTagIFRAMEend then
    NextToken;
end;

procedure TIpHtml.ParseNOFRAMES(Parent : TIpHtmlNode);
var
  CurNoFrames : TIpHtmlNodeNOFRAMES;
begin
  CurNoFrames := TIpHtmlNodeNOFRAMES.Create(Parent);
  NextToken;
  ParseBodyText(CurNoFrames, [IpHtmlTagNOFRAMESend, IpHtmlTagFRAMESETend]);
  if CurToken = IpHtmlTagNOFRAMESend then
    NextToken;
end;

procedure TIpHtml.ParseFrameSet(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
 {$IFDEF IP_LAZARUS_DBG} //JMN
  DebugLn('TIpHtml.ParseFrameSet A');
 {$ENDIF}
  FHasFrames := True;
  while CurToken = IpHtmlTagFRAMESET do begin
    FCurFrameSet := TIpHtmlNodeFRAMESET.Create(Parent);
    with FCurFrameSet do begin
      {ParseHyperMultiLengthList(htmlAttrROWS, '100%', FRows);}
      FRows := ParseHyperMultiLengthList(htmlAttrROWS, '100%');
      {ParseHyperMultiLengthList(htmlAttrCOLS, '100%', FCols);}
      FCols := ParseHyperMultiLengthList(htmlAttrCOLS, '100%');
      Id := FindAttribute(htmlAttrID);
      ClassId := FindAttribute(htmlAttrCLASS);
      Title := FindAttribute(htmlAttrTITLE);
      Style := FindAttribute(htmlAttrSTYLE);
    end;
    NextToken;
    if CurToken = IpHtmlTagFRAMESET then
      ParseFrameSet(FCurFrameSet, EndTokens + [IpHtmlTagFRAMESETend]);
    while CurToken = IpHtmlTagFRAME do
      ParseFrame(FCurFrameSet);
    if CurToken = IpHtmlTagNOFRAMES then
      ParseNOFRAMES(FCurFrameSet);
    if CurToken = IpHtmlTagFRAMESETend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseBody(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  i : Integer;
  Node : TIpHtmlNode;
begin
//  while CurToken = IpHtmlTagText do
//    NextToken;
  if CurToken = IpHtmlTagFRAMESET then begin
    ParseFrameSet(Parent, EndTokens);
    Exit;
  end;
  {lead token is optional}
  if CurToken = IpHtmlTagBODY then begin
    TIpHtmlNodeBODY.Create(Parent);
    with Body do begin
      BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
      TextColor := ColorFromString(FindAttribute(htmlAttrTEXT));
      Link := ColorFromString(FindAttribute(htmlAttrLINK));
      VLink := ColorFromString(FindAttribute(htmlAttrVLINK));
      ALink := ColorFromString(FindAttribute(htmlAttrALINK));
      Background := FindAttribute(htmlAttrBACKGROUND);
      ParseBaseProps(Self);
      {$IFDEF IP_LAZARUS}
      LoadAndApplyCSSProps;
      {$ENDIF}
    end;

    NextToken;
    ParseBodyText(Body, EndTokens + [IpHtmlTagBODYend]);
    EnsureClosure(IpHtmlTagBODYend, EndTokens);
  end else begin
//    Body := TIpHtmlNodeBODY.Create(Parent);
//    ParseBodyText(Body, EndTokens + [IpHtmlTagBODYend]);             
    ParseBodyText(Parent, EndTokens + [IpHtmlTagBODYend]);
    { Does the HTML include a body node? }
    if not TIpHtmlNodeHtml(Parent).HasBodyNode then
      { No. Create a body node under FHtml. }
      with TIpHtmlNodeHtml(Parent) do begin
        with TIpHtmlNodeBODY.Create(Parent) do
          {$IFDEF IP_LAZARUS}
          LoadAndApplyCSSProps;
          {$ENDIF};

        { Make each of FHtml's current children the children of the
          Body node. }
        for i := Pred(ChildCount) downto 0 do
          if ChildNode[i] <> Body then begin
            Node := ChildNode[i];
            FChildren.Remove(Node);
            Node.FParentNode := Body;
            Body.FChildren.Insert(0, Node);
          end;
      end;  { with }
    if CurToken = IpHtmlTagBODYend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseHtml;
begin
  {lead token is optional}
  if CurToken = IpHtmlTagHtml then begin
    HtmlNode.Version := FindAttribute(htmlAttrVERSION);
    HtmlNode.Lang := FindAttribute(htmlAttrLANG);
    HtmlNode.Dir := ParseDir;
    NextToken;
    ParseHead(HtmlNode); {may not be present}
    ParseBody(HtmlNode, [IpHtmlTagHtmlend, IpHtmlTagEOF]); {may not be present}
    if CurToken in [IpHtmlTagHtmlend, IpHtmlTagEOF] then
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagHtmlend);
    NextToken;
  end else begin
    ParseHead(HtmlNode); {may not be present}
    ParseBody(HtmlNode, [IpHtmlTagEof]); {may not be present}
  end;
end;

procedure TIpHtml.Parse;
{$IFDEF IP_LAZARUS}
var
  ch1,ch2,ch3: AnsiChar;
{$ENDIF}
begin
  Getmem(TokenStringBuf, 65536);
  try
    CharSP := 0;
    ListLevel := 0;
    StartPos := CharStream.Position;
    {$IFDEF IP_LAZARUS}
    FDocCharset := 'ISO-8859-1';
    FHasBOM := false;
    Ch1 := GetChar;
    Ch2 := GetChar;
    if (Ch1=#$FE) and (Ch2=#$FF) then begin
      FDocCharset := 'UCS-2BE';
      raise Exception.CreateFmt('%s document encoding not supported!',[FDocCharset]);
    end else
    if (Ch1=#$FF) and (ch2=#$FE) then begin
      FDocCharset := 'UCS-2LE';
      raise Exception.CreateFmt('%s document encoding not supported!',[FDocCharset]);
    end else
    if (Ch1=#$EF) and (ch2=#$BB) then begin
      Ch3 := GetChar;
      if Ch3=#$BF then begin
        FDocCharset := 'UTF-8';
        FHasBOM := true;
      end else begin
        PutChar(Ch3);
        PutChar(Ch2);
        PutChar(Ch1);
      end;
    end else begin
      PutChar(Ch2);
      PutChar(Ch1);
    end;
    {$ENDIF}
    repeat
      NextToken;
    until CurToken in [IpHtmlTagHtml, IpHtmlTagFRAMESET, IpHtmlTagEOF];
    if CurToken = IpHtmlTagEOF then begin
      CharStream.Position := StartPos;
      CharSP := 0;
      ListLevel := 0;
      repeat
        NextToken;
      until CurToken <> IpHtmlTagText;
    end;
    if CurToken = IpHtmlTagEOF then Exit;
    //ParseDocType; {may not be present}
    ParseHtml;
  finally
    FreeMem(TokenStringBuf);
    TokenStringBuf := nil;
    if ParmBuf <> nil then begin
      FreeMem(ParmBuf);
      ParmBuf := nil;
      ParmBufSize := 0;
    end;
  end;
end;

constructor TIpHtml.Create;
var
  TmpBitmap: TGraphic;
begin
  inherited Create;
  PropACache := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  PropBCache := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  DummyA := TIpHtmlPropA.Create;
  DummyA.UseCount := 1;
  DummyB := TIpHtmlPropB.Create(Self);
  DummyB.UseCount := 1;
  PropACache.Add(DummyA);
  PropBCache.Add(DummyB);
  ElementPool := TIpHtmlPoolManager.Create(sizeof(TIpHtmlElement), MaxElements);
  SoftLF := BuildStandardEntry(etSoftLF);
  HardLF := BuildStandardEntry(etHardLF);
  HardLFClearLeft := BuildStandardEntry(etClearLeft);
  HardLFClearRight := BuildStandardEntry(etClearRight);
  HardLFClearBoth := BuildStandardEntry(etClearBoth);
  FLIndent := BuildStandardEntry(etIndent);
  FLOutdent := BuildStandardEntry(etOutdent);
  SoftHyphen := BuildStandardEntry(etSoftHyphen);
  DefaultProps := TIpHtmlProps.Create(Self);
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
  AnchorList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  MapList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  AreaList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  MapImgList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  RectList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  FControlList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  LinkColor := clBlue;
  VLinkColor := clPurple;
  ALinkColor := clRed;
  {$IFDEF IP_LAZARUS}
  FCSS := TCSSGlobalProps.Create;
  FTabList := TIpHtmlTabList.Create;
    {$IFDEF UseGifImageUnit}
    GifImages := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
    {$ELSE}
    AnimationFrames := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
    {$ENDIF}
  {$ELSE}
  GifImages := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  OtherImages := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  {$ENDIF}
  NameList := TStringList.Create;
  DefaultImage := TPicture.Create;
  TmpBitmap := nil;
  try
    {$IFNDEF IP_LAZARUS}
    TmpBitmap := TBitmap.Create;
    TBitmap(TmpBitmap).LoadFromResourceName (HInstance, 'DEFAULTIMAGE');
    (**
    TmpBitmap.LoadFromResourceName(FindClassHInstance(
      TIpHTMLCustomPanel), 'DEFAULTIMAGE');
    **)
    {$ELSE}
    if LazarusResources.Find('DEFAULTIMAGE')<>nil then
      TmpBitmap := CreateBitmapFromLazarusResource('DEFAULTIMAGE')
    else
      TmpBitmap := CreateBitmapFromResourceName(HInstance, 'DEFAULTIMAGE');
    {$ENDIF}
    DefaultImage.Graphic := TmpBitmap;
  finally
    TmpBitmap.Free;
  end;
  GifQueue := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  FStartSel.x := -1;
  FEndSel.x := -1;
  //FixedTypeface := 'Courier New';
  FBgColor := -1;
  FFactBAParag := 1;
end;

function TIpHtml.LinkVisited(const URL : string): Boolean;
begin
  if (length(URL) > 0) and (URL[1] = '#') then
    Result := True
  else
    Result := CheckKnownURL(URL);
end;

{$IFOPT C+}
procedure TIpHtml.CheckImage(Picture: TPicture);
begin
  if Picture <> nil then begin
    if not (Picture is TPicture) then
      raise EIpHtmlException.Create(SHTMLInvPicture);
    if Picture.Graphic = nil then
      raise EIpHtmlException.Create(SHTMLNoGraphic);
    if not (Picture.Graphic is TGraphic) then
      raise EIpHtmlException.Create(SHTMLInvGraphic);
  end;
end;
{$ENDIF}

procedure TIpHtml.DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
begin
  if assigned(FOnGetImageX) then
    OnGetImageX(Sender, URL, Picture)
  else
    raise EIpHtmlException.Create(SHTMLNoGetImage);
  {$IFOPT C+}
  CheckImage(Picture);
  {$ENDIF}
end;

procedure TIpHtml.FinalizeRecs(P: Pointer);
begin
  {$IFDEF IP_LAZARUS}
  with PIpHtmlElement(P)^ do begin
    //ElementType : TElementType;
    AnsiWord:='';
    //IsBlank : Integer;
    //SizeProp: TIpHtmlPropA;
    //Size: TSize;
    //WordRect2 : TRect;
    //Props : TIpHtmlProps;
    //Owner : TIpHtmlNode;
  end;
  {$ELSE}
  Finalize(PIpHtmlElement(P)^);
  {$ENDIF}
end;

destructor TIpHtml.Destroy;
var
  i : Integer;
begin
 {$IFDEF IP_LAZARUS} //JMN
 FCSS.Free;
    {$IFDEF UseGifImageUnit}
    for i := 0 to Pred(GifImages.Count) do
      if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
        TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
    {$ELSE}
    for i := 0 to Pred(AnimationFrames.Count) do
      if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
        TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic).
          AggressiveDrawing := False;
    {$ENDIF}
 {$ELSE}
  for i := 0 to Pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
  for i := 0 to Pred(OtherImages.Count) do
    if TIpHtmlNodeIMG(OtherImages[i]).FPicture <> nil then
      TIpHtmlNodeIMG(OtherImages[i]).FPicture.Graphic := nil;
 {$ENDIF}
  Destroying := True;
  PaintBufferBitmap.Free;
  ClearGifQueue;
  Clear;
  GifQueue.Free;
  DefaultImage.Free;
  NameList.Free;
  FHtml.Free;
  AnchorList.Free;
  MapList.Free;
  AreaList.Free;
  ClearRectList;
  RectList.Free;
  MapImgList.Free;
  FControlList.Free;
  DefaultProps.Free;
 {$IFDEF IP_LAZARUS}
  FTabList.Free;
  {$IFDEF UseGifImageUnit}
  GifImages.Free;
  {$ELSE}
  AnimationFrames.Free;
  {$ENDIF}
 {$ELSE}
  GifImages.Free;
  OtherImages.Free;
 {$ENDIF}
  ElementPool.EnumerateItems(FinalizeRecs);
  ElementPool.Free;
  ClearCache;
  inherited;
end;

function TIpHtml.ParseFrameProp(Default : TIpHtmlFrameProp): TIpHtmlFrameProp;
var
  S : string;
begin
  Result := hfVoid;
  S := UpperCase(FindAttribute(htmlAttrFRAME));
  if length(S) = 0 then
  begin
    Result := Default;
    exit;
  end;
  case S[1] of
    'A': if (S = 'ABOVE') then Result := hfAbove;
    'B': if S = 'BELOW' then Result := hfBelow
         else if S = 'BOX' then Result := hfBox
         else if S = 'BORDER' then Result := hfBorder;
    'H': if S = 'HSIDES' then Result := hfHSides;
    'L': if S = 'LHS' then Result := hfLhs;
    'R': if S = 'RHS' then Result := hfRhs;
    'V': if (S = 'VOID') then exit
         else if S = 'VSIDES' then
           Result := hfvSides;
    else
      if FlagErrors then
        ReportError(SHtmlInvFrame);
  end;
end;

function TIpHtml.ParseRules(Default : TIpHtmlRules): TIpHtmlRules;
var
  S : string;
begin
  Result := hrNone;
  S := UpperCase(FindAttribute(htmlAttrRULES));
  if length(S) = 0 then
  begin
    Result := Default;
    exit;
  end;
  case S[1] of
    'A': if S = 'ALL' then Result := hrAll;
    'C': if S = 'COLS' then Result := hrCols;
    'G': if S = 'GROUPS' then Result := hrGroups;
    'N': if S = 'NONE' then exit;
    'R': if S = 'ROWS' then Result := hrRows;
    else
      if FlagErrors then
        ReportError(SHtmlInvRule);
  end;
end;

function TIpHtml.ParseCellAlign(Default : TIpHtmlAlign): TIpHtmlAlign;
begin
  Result := GetAlignmentForStr(FindAttribute(htmlAttrALIGN), haCenter);
//  if FlagErrors then
//    ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseFrameScrollingProp: TIpHtmlFrameScrolling;
var
  S : string;
begin
  Result := hfsAuto;
  S := UpperCase(FindAttribute(htmlAttrSCROLLING));
  if (length(S) = 0) then exit;
  case S[1] of
    'A': if (S = 'AUTO') then exit;
    'N': if S = 'NO' then Result := hfsNo;
    'Y': if S = 'YES' then Result := hfsYes;
    else
      if FlagErrors then
        ReportError(SHtmlInvScroll);
  end;
end;

function TIpHtml.ParseVAlignment3: TIpHtmlVAlign3;
var
  S : string;
begin
  Result := hva3Middle;
  S := UpperCase(FindAttribute(htmlAttrVALIGN));
  if length(S) = 0 then
  begin
    Result := hva3Default;
    exit;
  end;
  case S[1] of
    'B': if S = 'BOTTOM' then Result := hva3Bottom
         else if S = 'BASELINE' then Result := hva3Baseline;
    'C','M': if (S = 'MIDDLE') or (S = 'CENTER') then exit;
    'T': if (S = 'TOP') then Result := hva3Top;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

procedure TIpHtml.SetDefaultProps;
begin
  if FDefaultTypeFace='' then begin
    {$IFDEF MSWindows}
    Defaultprops.FontName := 'Times New Roman';
    {$ELSE}
    Defaultprops.FontName := Graphics.DefFontData.Name
    {$ENDIF}
  end else
    Defaultprops.FontName := FDefaultTypeface;
  Defaultprops.FontSize := FDefaultFontSize;
  DefaultProps.BaseFontSize := 3;
  Defaultprops.FontBaseline := 0;
  DefaultProps.VAlignment := hva3Baseline;
  Defaultprops.FontStyle := [];
  Defaultprops.Alignment := haLeft;
  DefaultProps.FontColor := TextColor;
  DefaultProps.LinkColor := LinkColor;
  DefaultProps.VLinkColor := VLinkColor;
  DefaultProps.ALinkColor := ALinkColor;
  DefaultProps.BgColor := BgColor;
  DefaultProps.Preformatted := False;
  DefaultProps.NoBreak := False;
  if Body <> nil then begin
    if Body.TextColor <> -1 then
      DefaultProps.FontColor := Body.TextColor;
    if Body.Link <> -1 then
      DefaultProps.LinkColor := Body.Link;
    if Body.VLink <> -1 then
      DefaultProps.VLinkColor := Body.VLink;
    if Body.ALink <> -1 then
      DefaultProps.ALinkColor := Body.ALink;
    if Body.BgColor <> -1 then
      DefaultProps.BgColor := Body.BgColor;
  end;
end;

function TIpHtml.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with PageViewRect do begin
    Dec(Result.x, Left);
    Dec(Result.y, Top);
  end;
  with ClientRect do begin
    Inc(Result.x, Left);
    Inc(Result.y, Top);
  end;
end;

function TIpHtml.PageRectToScreen(const Rect: TRect; var ScreenRect: TRect): Boolean;
{-convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    Exit;
  end;
  if not IntersectRect(Tmp, Rect, PageViewRect) then begin
    Result := False;
    Exit;
  end;
  ScreenRect := Rect;
  with PageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with ClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, ClientRect) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

{$IFDEF IP_LAZARUS}
function TIpHtml.GetSelectionBlocks(out StartSelIndex,EndSelIndex: Integer): boolean;
var
  R : TRect;
  CurBlock: TIpHtmlNodeBlock;
begin
  Result := false;

  if not AllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then Exit;
  

  if not AllSelected then begin
    CurBlock := nil;
    // search blocks that intersect the selection
    // 1.- find first block that intersect upleft  point of sel. (start from 0)
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if AllSelected and (CurBlock <> nil) then
        break;}
      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          // block within selection (vertically)
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          // selection start or ends in this block
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then Exit;
    // 2.- find first block thta intersect downright point of sel. (start from count-1)
    EndSelIndex := Pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
      if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if AllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  Result := True;
end;
{$ENDIF}

function TIpHtml.getControlCount:integer;
begin
     result := FControlList.Count;
end;

function TIpHtml.getControl(i:integer):TIpHtmlNode;
begin
     result := FControlList[i];
end;

procedure TIpHtml.PaintSelection;
var
  StartSelIndex, EndSelIndex,
  i : Integer;
  R : TRect;
  CurBlock: TIpHtmlNodeBlock;
begin
  if not AllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then Exit;
  if not AllSelected then begin
    CurBlock := nil;
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if AllSelected and (CurBlock <> nil) then
        break;}
      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then Exit;
    EndSelIndex := Pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
      if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if AllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  for i := StartSelIndex to EndSelIndex do begin
    R := PIpHtmlRectListEntry(RectList[i]).Rect;
    if PageRectToScreen(R, R) then begin
      {$IFDEF IP_LAZARUS}
      DebugLn('TIpHtml.PaintSelection  PatBlt not implemented');
      {$ELSE}
      PatBlt(PaintBuffer.Handle, R.Left, R.Top,
        R.Right - R.Left, R.Bottom - R.Top, DSTINVERT);
      {$ENDIF}
    end;
  end;
end;

procedure TIpHtml.RequestImageNodes(Node : TIpHtmlNode);
var
  i : Integer;
begin
  if Node is TIpHtmlNodeIMG then begin
    if TIpHtmlNodeIMG(Node).FPicture = nil then
      TIpHtmlNodeIMG(Node).LoadImage;
  end;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do begin
      RequestImageNodes(TIpHtmlNodeMulti(Node).ChildNode[i]);
    end;
end;

{$IFDEF IP_LAZARUS_DBG}
var
  CCC: Integer;
  
procedure TIpHtml.DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
var
  i: Integer;
begin
  if Node=UserData then
    Write('Parent: ');
  for i:=0 to CCC do Write(' ');
  Write('Node: ', Node.ClassName);
  if Node is TIpHtmlNodeText then
    Write(' ', TIpHtmlNodeText(NodE).ANSIText);
  WriteLn;
  if Node=UserData then
    Exit;
  Inc(CCC);
  Node.EnumChildren(DebugChild, Node);
  Dec(CCC);
end;

procedure TIpHtml.DebugAll;
//var
  //i: Integer;
  //item: PIpHtmlRectListEntry;
  //Node: TIpHtmlNode;
begin
  CCC := 0;
  Fhtml.EnumChildren(DebugChild, FHtml);
  {
  
  for i:=0 to RectList.Count-1 do begin
    WriteLn('RectList[',i,']:');
    Item := PIpHtmlRectListEntry(Rectlist[i]);
    if Item<>nil then begin
      WriteLn(' Node=', dbgs(Item.Node));
      WriteLn('   Owner=', dbgs(Item.Node^.Owner));
      WriteLn('    Text=', Item.Node^.AnsiWord);
      Node := Item.Node^.Owner;
      if Node<>nil then begin
      WriteLn('     ClassName:', Node.ClassName);
        if Node is TIpHtmlNodeText then
          WriteLn('       Text=', TIpHtmlNodeText(Node).ANSIText);
      end;
      WriteLn(' Block=', dbgs(Item.Block));
      WriteLn(' Rect=', dbgs(Item.Rect));
    end;
  end;
  }
end;
{$ENDIF}

procedure TIpHtml.Render(TargetCanvas: TCanvas; TargetPageRect : TRect;
  UsePaintBuffer: Boolean; const TopLeft: TPoint);
var
  i : Integer;
begin
  ClientRect.TopLeft := TopLeft; {Point(0, 0);}
  ClientRect.Right := TargetPageRect.Right - TargetPageRect.Left;
  ClientRect.Bottom := TargetPageRect.Bottom - TargetPageRect.Top;
  if not DoneLoading then begin
    TargetCanvas.FillRect(ClientRect);
    Exit;
  end;
 {$IFDEF IP_LAZARUS}
    {$IFDEF UseGifImageUnit}
    for i := 0 to Pred(GifImages.Count) do
      if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
        with TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic) do
          if Painters <> nil then
            PaintStop;
    {$ELSE}
    for i := 0 to Pred(AnimationFrames.Count) do
      if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
        with TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic) do
          AggressiveDrawing := False;
    {$ENDIF}
 {$ELSE}
  for i := 0 to Pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      with TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic) do
        if Painters <> nil then
          PaintStop;
 {$ENDIF}
  for i := 0 to Pred(FControlList.Count) do
    TIpHtmlNode(FControlList[i]).UnmarkControl;
  SetDefaultProps;
  PageViewRect := TargetPageRect;
  if UsePaintBuffer then begin
    if (PaintBuffer = nil)
    or (PaintBufferBitmap.Width <> Clientrect.Right)
    or (PaintBufferBitmap.Height <> ClientRect.Bottom) then begin
      PaintBufferBitmap.Free;
      PaintBufferBitmap := TBitmap.Create;
      PaintBufferBitmap.Width := ClientRect.Right;
      PaintBufferBitmap.Height := ClientRect.Bottom;
      PaintBuffer := PaintBufferBitmap.Canvas;
    end;
    FTarget := PaintBuffer;
  end else begin
    PaintBuffer := TargetCanvas;
    FTarget := TargetCanvas;
  end;
  ClearRectList;
  if FHtml <> nil then
    FHtml.Render(DefaultProps);

  for i := 0 to Pred(FControlList.Count) do
    TIpHtmlNode(FControlList[i]).HideUnmarkedControl;
  {$IFNDEF IP_LAZARUS}
  PaintSelection;
  {$ENDIF}
  if UsePaintBuffer then
    TargetCanvas.CopyRect(ClientRect, PaintBuffer, ClientRect)
  else
    if PaintBufferBitmap <> nil then
      PaintBuffer := PaintBufferBitmap.Canvas
    else
      PaintBuffer := nil;
  StartGifPaint(TargetCanvas);
  {Request all non-visible images}
  RequestImageNodes(HtmlNode);
end;

procedure TIpHtml.ResetElementMetrics(P: Pointer);
begin
  with PIpHtmlElement(P)^ do begin
    Size.cx := 0;
    Size.cy := 0;
    WordRect2 := Rect(0, 0, 0, 0);
    SizeProp := nil;
  end;
end;

procedure TIpHtml.ResetWordLists;
begin
  ElementPool.EnumerateItems(ResetElementMetrics);
end;

procedure TIpHtml.ResetBlocks(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then Exit;
  if Node is TIpHtmlNodeBlock then
    with TIpHtmlNodeBlock(Node) do begin
      InvalidateSize;
    end
  else
  if Node is TIpHtmlNodeTable then
    with TIpHtmlNodeTable(Node) do begin
      FMin := -1;
      FMax := -1;
    end;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetBlocks(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.ResetImages(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then Exit;
  if Node is TIpHtmlNodeIMG then
    with TIpHtmlNodeIMG(Node) do begin
      {UnloadImage;}
      InvalidateSize;
    end
  else
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetImages(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.ResetCanvasData;
begin
  ResetCache;
  ResetWordLists;
  ResetBlocks(FHtml);
  ResetImages(FHtml);
end;

function TIpHtml.GetPageRect(TargetCanvas: TCanvas; Width, Height : Integer): TRect;
var
  DefPageRect : TRect;
  Min, Max, W, H : Integer;
begin
  //debugln(['TIpHtml.GetPageRect START DoneLoading=',DoneLoading,' FHtml=',FHtml<>nil]);
  if not DoneLoading then begin
    {$IFDEF IP_LAZARUS}
    // always set Result
    SetRectEmpty(Result);
    {$ENDIF}
    Exit;
  end;
  DoneLoading := False;
  SetRectEmpty(FPageRect);
  if FHtml <> nil then begin
    if (TargetCanvas <> RenderCanvas)
    or (PageHeight <> Height) then
      ResetCanvasData;
    PageHeight := Height;
    SetDefaultProps;
    {PanelWidth := Width;}
    FTarget := TargetCanvas;
    FHtml.CalcMinMaxHtmlWidth(DefaultProps, Min, Max);
    //debugln(['TIpHtml.GetPageRect Min=',Min,' Max=',Max]);
    W := MaxI2(Min + 2 * MarginWidth, Width);
    H := FHtml.GetHeight(DefaultProps, W - 2 * MarginWidth) + 2 * MarginHeight;
    DefPageRect := Rect(
      MarginWidth,
      MarginHeight,
      W - MarginWidth,
      H - MarginHeight);
    ClearAreaLists;
    ClearAreaList;
    FHtml.Layout(DefaultProps, DefPageRect);
    FPageRect := DefPageRect;
    FPagerect.Bottom := FPageRect.Bottom + MarginHeight;
    FPageRect.Right := FPageRect.Right + MarginWidth;
    RenderCanvas := TargetCanvas;
  end;
  Result := FPageRect;
  DoneLoading := True;
end;

procedure TIpHtml.InvalidateSize;
begin
  if assigned(FOnInvalidateSize) then
    FOnInvalidateSize(Self);
end;

procedure TIpHtml.ClearAreaList;
var
  i : Integer;
begin
  for i := 0 to Pred(AreaList.Count) do
    TIpHtmlNodeArea(AreaList[i]).Reset;
  AreaList.Clear;
end;

function RectFromString(const S: string): TRect;
var
  i, j, x, err : Integer;

  procedure Next;
  begin
    j := i;
    while (j <= length(S)) and (S[j] <> ',') do
      Inc(j);
    val(copy(S, i, j - i), x, err);
  end;

begin
  SetRectEmpty(Result);
  i := 1;
  Next;
  if err <> 0 then Exit;
  Result.Left := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Top := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Right := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Bottom := x;
end;

function CircularRegion(const Coords: string; const Rect: TRect): HRgn;
var
  i, j, err, cx, cy, R : Integer;
begin
  Result := 0;
  i := 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    Inc(j);
  val(copy(Coords, i, j - i), cx, err);
  if err <> 0 then Exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    Inc(j);
  val(copy(Coords, i, j - i), cy, err);
  if err <> 0 then Exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') and (Coords[j] <> '%') do
    Inc(j);
  val(copy(Coords, i, j - i), R, err);
  if err <> 0 then Exit;
  if (j <= length(Coords)) and (Coords[j] = '%') then
    R := round(R * MinI2(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top) / 100);
  if R < 1 then Exit;
  Result := CreateEllipticRgn(
    Rect.Left + cx - R,
    Rect.Top + cy - R,
    Rect.Left + cx + R,
    Rect.Top + cy + R);
end;

function PolygonRegion(const Coords: string; const Rect: TRect): HRgn;
const
  MAXPOINTS = 4096;
var
  Points : array [0.. Pred(MAXPOINTS)] of TPoint;
  Count, i, j, x, y, err : Integer;
begin
  Result := 0;
  Count := 0;
  i := 1;
  while i < length(Coords) do begin
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      Inc(j);
    val(copy(Coords, i, j - i), x, err);
    if err <> 0 then Exit;
    i := j + 1;
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      Inc(j);
    val(copy(Coords, i, j - i), y, err);
    if err <> 0 then Exit;
    Points[Count].x := x + Rect.Left;
    Points[Count].y := y + Rect.Top;
    Inc(Count);
    i := j + 1;
  end;
  if Count < 3 then Exit;
  if (Points[0].x <> Points[Count - 1].x)
  or (Points[0].y <> Points[Count - 1].y) then begin
    Points[Count] := Points[0];
    Inc(Count);
  end;
  Result := CreatePolygonRgn(
    {$IFDEF IP_LAZARUS}
    PPoint(@Points[0]),
    {$ELSE}
    (@Points[0])^,
    {$ENDIF}
    Count,
    ALTERNATE); {fill mode is irrelevant here}
end;

procedure TIpHtml.BuildAreaList;
var
  i, j, k : Integer;
  R, R2 : TRect;
begin
  ClearAreaList;
  for i := 0 to Pred(MapImgList.Count) do
    with TIpHtmlNodeIMG(MapImgList[i]) do begin
      R := GrossDrawRect;
      for j := 0 to Pred(MapList.Count) do
        with TIpHtmlNodeMap(MapList[j]) do begin
          for k := 0 to Pred(FChildren.Count) do
            if TIpHtmlNode(FChildren[k]) is TIpHtmlNodeArea then begin
              with TIpHtmlNodeArea(FChildren[k]) do begin
                if HRef <> '' then begin
                  case Shape of
                  hmsDefault :
                    FRect := R;
                  hmsRect :
                    begin
                      R2 := RectFromString(Coords);
                      OffsetRect(R2, R.Left, R.Top);
                      FRect := R2;
                    end;
                  hmsCircle :
                    FRgn := CircularRegion(Coords, R);
                  hmsPoly :
                    FRgn := PolygonRegion(Coords, R);
                  end;
                end;
              end;
              AreaList.Add(TIpHtmlNodeArea(FChildren[k]));
            end;
        end;
    end;
end;

procedure TIpHtml.MouseMove(Pt: TPoint);
var
  i : Integer;
begin
  FMouseLastPoint := Pt;
  FHotPoint := Point(-1, -1);
  if (MapList.Count > 0) and (AreaList.Count = 0) then
    BuildAreaList;
  for i := 0 to Pred(AnchorList.Count) do
    if TIpHtmlNodeA(AnchorList[i]).PtInRects(Pt) then begin
      if FHotNode <> TIpHtmlNodeA(AnchorList[i]) then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
           TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AnchorList[i]);
        if FHotNode is TIpHtmlNodeA then
          TIpHtmlNodeA(FHotNode).Hot := True;
      end;
      if (FHotNode <> nil) then
        if FHotNode is TIpHtmlNodeA then
          FHotPoint := TIpHtmlNodeA(FHotNode).RelMapPoint(Pt);
      Exit;
    end;
  for i := 0 to Pred(AreaList.Count) do
    if TIpHtmlNodeAREA(AreaList[i]).PtInRects(Pt) then begin
      if FHotNode <> AreaList[i] then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
            TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AreaList[i]);
      end;
      Exit;
    end;
  if FHotNode <> nil then
    if FHotNode is TIpHtmlNodeA then
      TIpHtmlNodeA(FHotNode).Hot := False;
  FHotNode := nil;
  FCurElement := nil;
  for i := 0 to Pred(RectList.Count) do
    if PtInRect(PIpHtmlRectListEntry(RectList[i]).Rect, Pt) then begin
      FCurElement := PIpHtmlRectListEntry(RectList[i]).Node;
      break;
    end;
end;

function TIpHtml.BuildPath(const Ext: string): string;
begin
  {$IFDEF IP_LAZARUS}
  if FDataProvider <> nil then
    Result := FDataProvider.BuildURL(FCurURL,Ext)
  else
  {$ENDIF}
  Result :=  BuildURL(FCurURL, Ext);
end;

function TIpHtml.NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
begin
  Result := ElementPool.NewItm;
  Result.ElementType := EType;
  Result.Owner := Own;
  {$IFDEF IP_LAZARUS}
  Result.IsSelected := False;
  {$ENDIF}
end;

function TIpHtml.BuildStandardEntry(EType: TElementType): PIpHtmlElement;
begin
  Result := NewElement(EType, nil);
  Result.Props := nil;
  SetWordRect(Result, Rect(0, 0, 0, 0));
end;

procedure TIpHtml.MakeVisible(const R: TRect{$IFDEF IP_LAZARUS}; ShowAtTop: Boolean = True{$ENDIF});
begin
  if assigned(FOnScroll) then
    FOnScroll(Self, R{$IFDEF IP_LAZARUS}, ShowAtTop{$ENDIF});
end;

function TIpHtml.FindElement(const Name: string): TIpHtmlNode;
var
  i : Integer;
begin
  NameList.Sorted := True;
  i := NameList.IndexOf(Name);
  if i <> -1 then
    Result := TIpHtmlNode(NameList.Objects[i])
  else
    Result := nil;
end;

type
  TIpHtmlGifQueueEntry = class
  protected
    FGraphic : TGraphic;
    FR : TRect;
  public
    constructor Create(AGraphic: TGraphic; ARect: TRect);
    property Graphic : TGraphic read FGraphic;
    property R : TRect read FR;
  end;

procedure TIpHtml.ClearAreaLists;
var
  i : Integer;
begin              
  for i := 0 to Pred(AnchorList.Count) do
    TIpHtmlNodeA(AnchorList[i]).ClearAreaList;
end;

procedure TIpHtml.Home;
begin
  MakeVisible(Rect(0, 0, 1, 1));
end;

procedure TIpHtml.Get(const URL: string);
begin
  if assigned(FOnGet) then
    FOnGet(Self, URL);
end;

procedure TIpHtml.Post(const URL: string; FormData: TIpFormDataEntity);
begin
  if assigned(FOnPost) then
    FOnPost(Self, URL, FormData);
end;

procedure TIpHtml.AddRect(const R : TRect; Node : PIpHtmlElement; Block: TIpHtmlNodeBlock);
var
  NewEntry : PIpHtmlRectListEntry;
begin
  New(NewEntry);
  NewEntry.Rect := R;
  NewEntry.Node := Node;
  NewEntry.Block := Block;
  RectList.Add(NewEntry);
end;

procedure TIpHtml.ClearRectList;
var
  i : Integer;
  p: PIpHtmlRectListEntry;
begin
  for i := Pred(RectList.Count) downto 0 do begin
    p:=PIpHtmlRectListEntry(RectList[i]);
    Freemem(p);
  end;
  RectList.Clear;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtml.DeselectAllItems(Item: Pointer);
begin
  PIpHtmlElement(item)^.IsSelected := False;
end;
{$ENDIF}

procedure TIpHtml.SetSelection(StartPoint, EndPoint: TPoint);
{$IFDEF IP_LAZARUS}
var
  StartSelIndex,EndSelindex: Integer;
  i: Integer;
  r: TRect;
  Selected: boolean;
  DeselectAll: boolean;
  item: PIpHtmlRectListEntry;
{$ENDIF}
begin
  {$IFDEF IP_LAZARUS}
  if AllSelected then
    InvalidateRect(Body.PageRect);
  {$ENDIF}
  AllSelected := False;
  if EndPoint.y > StartPoint.y then begin
    FStartSel := StartPoint;
    FEndSel := EndPoint;
  end
  else
  if EndPoint.y = StartPoint.y then
    if EndPoint.x > StartPoint.x then begin
      FStartSel := StartPoint;
      FEndSel := EndPoint;
    end else begin
      FStartSel := EndPoint;
      FEndSel := StartPoint;
    end
  else begin
    FStartSel := EndPoint;
    FEndSel := StartPoint;
  end;
  {$IFDEF IP_LAZARUS}
  if Body <> nil then begin
    // Invalidate only those blocks that need it
    DeselectAll := (EndPoint.x<0)and(EndPoint.y<0);
    GetSelectionBlocks(StartSelIndex,EndSelIndex);
    for i:= 0 to RectList.Count-1 do begin
      item := PIpHtmlRectListEntry(RectList[i]);
      // (de)select only text elements
      if Item.Node.ElementType<>etWord then
        Continue;
      if DeselectAll then
        Selected := false
      else
        Selected := (StartSelIndex<=i)and(i<=EndSelIndex);
      // Invalidate only changed elements
      if Item.Node.IsSelected<>Selected then begin
        Item.Node.IsSelected := Selected;
        if Body.PageRectToScreen(Item^.Rect, R) then
          InvalidateRect(R);
      end;
    end;
    // also deselect remaining elements
    if DeselectAll then
      ElementPool.EnumerateItems(DeselectAllItems);
  end;
  {$ELSE}
  if Body <> nil then
    InvalidateRect(Body.PageRect);
  {$ENDIF}
end;

procedure TIpHtml.SelectAll;
begin
  AllSelected := True;
end;

procedure TIpHtml.DeselectAll;
begin
  AllSelected := False;
  FStartSel.x := -1;
  FEndSel.x := -1;
end;

procedure TIpHtml.CopyToClipboard;
var
  S : string;
begin
  if HaveSelection then begin
    S := '';
    if FHtml <> nil then
      FHtml.AppendSelection(S);
    if S <> '' then begin
      Clipboard.Open;
      try
        Clipboard.Clear;
        Clipboard.AsText := S;
      finally
        Clipboard.Close;
      end;
    end;
  end;
end;

function TIpHtml.HaveSelection: Boolean;
begin
  Result := AllSelected or ((FEndSel.x > 0) or (FEndSel.y > 0));
end;

procedure TIpHtml.CreateIFrame(Parent: TWinControl; Frame: TIpHtmlNodeIFRAME;
  var Control: TWinControl);
begin
  if assigned(FOnIFrameCreate) then
    FOnIFrameCreate(Self, Parent, Frame, Control);
end;

function TIpHtml.CheckKnownURL(URL: string): boolean;
var
  P : Integer;
begin
  if assigned(FOnURLCheck) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      SetLength(URL, P - 1);
    {$IFDEF IP_LAZARUS}
    Result:=true;
    {$ENDIF}
    FOnURLCheck(Self, URL, Result);
  end;
end;

procedure TIpHtml.ReportReference(URL: string);
var
  P : Integer;
begin
  if assigned(FOnReportURL) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      if P = 1 then
        Exit
      else
        SetLength(URL, P - 1);
    FOnReportURL(Self, URL);
  end;
end;

procedure TIpHtml.ControlClick(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, Sender);
end;

procedure TIpHtml.ControlClick2(Sender: TIpHtmlNodeControl; var cancel: boolean);
begin
  if assigned(FControlClick2) then
    FControlClick2(Self, Sender, cancel);
end;

procedure TIpHtml.ControlOnEditingDone(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlOnEditingDone) then
    FControlOnEditingDone(Self, Sender);
end;

procedure TIpHtml.ControlOnChange(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlOnChange) then
    FControlOnChange(Self, Sender);
end;

procedure TIpHtml.ControlCreate(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, Sender);
end;

{ TIpHtmlGifQueueEntry }

constructor TIpHtmlGifQueueEntry.Create(AGraphic: TGraphic; ARect: TRect);
begin
  {$IFDEF IP_LAZARUS_DBG}
  DebugLn('TIpHtmlGifQueueEntry.Create ToDo NOT IMPLEMENTED YET');
  {$ELSE}
  FGraphic := AGraphic;
  {$ENDIF}
  FR := ARect;
end;

procedure TIpHtml.AddGifQueue(Graphic: TGraphic; const R: TRect);
begin
  GifQueue.Add(TIpHtmlGifQueueEntry.Create(Graphic, R));
end;

procedure TIpHtml.StartGifPaint(Target: TCanvas);
var
  i : Integer;
begin
  for i := 0 to Pred(GifQueue.Count) do
    with TIpHtmlGifQueueEntry(GifQueue[i]) do
      Target.StretchDraw(R, Graphic);
  ClearGifQueue;
end;

procedure TIpHtml.ClearGifQueue;
var
  i : Integer;
begin
  if Assigned(GifQueue) then
    for i := Pred(GifQueue.Count) downto 0 do begin
      TIpHtmlGifQueueEntry(GifQueue[i]).Free;
      GifQueue.Delete(i);
    end;
end;

{ TIpHtmlNodeText }
//var
//   NodetextCount : integer = 0;

constructor TIpHtmlNodeText.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  PropsR := TIpHtmlProps.Create(Owner);
//  inc(NodetextCount);
//  DebugLn('NodeText: ', InttoStr(NodetextCount));
end;

destructor TIpHtmlNodeText.Destroy;
begin
  inherited;
  PropsR.Free;
end;

procedure TIpHtmlNodeText.SetProps(const RenderProps: TIpHtmlProps);
begin
  PropsR.Assign(RenderProps);
end;

procedure TIpHtmlNodeText.Enqueue;
begin
  BuildWordList;
end;

procedure TIpHtmlNodeText.AddAWord(StartP: PAnsiChar);
begin
  if FFirstW then
    Owner.AddWord(StartP, PropsR, Self)
  else
    Owner.AddWord(StartP, nil, Self);
  FFirstW := False;
end;

function TIpHtmlNodeText.CutAndAddWord(StartP, EndP: PAnsiChar): PAnsiChar;
var
  EndCh: AnsiChar;
begin
  EndCh := EndP^;
  EndP^ := #0;
  AddAWord(StartP);
  EndP^ := EndCh;
  Result := EndP;
end;

procedure TIpHtmlNodeText.DoPreformattedWords(N: PAnsiChar);
var
  N2: PAnsiChar;
  ImplicitLF: Boolean;
begin
  ImplicitLF := False;
  while N^ <> #0 do begin
    case N^ of
    CR :
      ImplicitLF := True;
    LF :
      begin
        EnqueueElement(Owner.HardLF);
        Inc(N);
        ImplicitLF := False;
      end;
    else
      begin
        if ImplicitLF then begin
          EnqueueElement(Owner.HardLF);
          Inc(N);
          ImplicitLF := False;
        end;
        N2 := StrScan(N, CR);
        if N2 <> nil then
          N := CutAndAddWord(N, N2)
        else begin
          N2 := StrScan(N, LF);
          if N2 <> nil then
            N := CutAndAddWord(N, N2)
          else begin
            AddAWord(N);
            N^ := #0;
          end;
        end;
      end;
    end;
  end;
end;

procedure TIpHtmlNodeText.DoNormalWords(N: PAnsiChar);
var
  NewEntry : PIpHtmlElement;
  N2: PAnsiChar;
begin
  while N^ <> #0 do begin
    case N^ of
    LF :
      begin
        EnqueueElement(Owner.HardLF);
        Inc(N);
      end;
    ' ' :
      begin
        if not ElementQueueIsEmpty then begin
          NewEntry := Owner.NewElement(etWord, Self);
          NewEntry.AnsiWord := ' ';
          NewEntry.IsBlank := 1;
          if FFirstW then
            NewEntry.Props := PropsR
          else
            NewEntry.Props := nil;
          EnqueueElement(NewEntry);
          FFirstW := False;
        end;
        Inc(N);
      end;
    else
      begin
        N2 := N;
        while not (N2^ in [#0, ' ', LF]) do
          Inc(N2);
        if N2^ <> #0 then
          N := CutAndAddWord(N, N2)
        else begin
          AddAWord(N);
          N^ := #0;
        end;
      end;
    end;
  end;
end;

procedure TIpHtmlNodeText.BuildWordList;
var
  l : Integer;
  B : PAnsiChar;
begin
  FFirstW := True;
  l := length(EscapedText);
  if l > 0 then begin
    Getmem(B, l + 1);
    try
      TrimFormatting(EscapedText, B, PropsR.Preformatted);
      if PropsR.Preformatted then
        DoPreformattedWords(B)
      else
        DoNormalWords(B);
    finally
      FreeMem(B);
    end;
  end;
end;

function TIpHtmlNodeText.GetAnsiText: string;
begin
  Result := EscapeToAnsi(FEscapedText);
end;

procedure TIpHtmlNodeText.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FParentNode.EnqueueElement(Entry);
end;

function FindInnerBlock(Node : TIpHTMLNode): TIpHtmlNodeBlock;
begin
  while not (Node is TIpHtmlNodeBlock) do
    Node := Node.FParentNode;
  Result := TIpHtmlNodeBlock(Node);
end;

procedure TIpHtmlNodeText.SetAnsiText(const Value: string);
begin
  EscapedText := AnsiToEscape(Value);
end;

procedure TIpHtmlNodeText.SetEscapedText(const Value: string);
var
  Block: TIpHtmlNodeBlock;
begin
  FEscapedText := Value;
  Block := FindInnerBlock(Self);

  {we need to clear the queue so that it will be built again}
  Block.ClearWordList;

  {then, we need to Invalidate the block so that
   the rendering logic recalculates everything}
  Block.InvalidateSize;
end;

procedure TIpHtmlNodeText.ReportDrawRects(M: TRectMethod);
begin
  ReportCurDrawRects(Self, M);
end;

function TIpHtmlNodeText.ElementQueueIsEmpty: Boolean;
begin
  Result := FParentNode.ElementQueueIsEmpty;
end;

{ TIpHtmlNodeFONT }

procedure TIpHtmlNodeFONT.ApplyProps(const RenderProps: TIpHtmlProps);
var
  TmpSize : Integer;
begin
  Props.Assign(RenderProps);
  if Face <> '' then
    Props.FontName := FirstString(Face);
  case Size.SizeType of
  hrsAbsolute :
    Props.FontSize := FONTSIZESVALUSARRAY[Size.Value-1];
  hrsRelative :
    begin
      TmpSize := Props.BaseFontSize + Size.Value;
      if TmpSize <= 1 then
        Props.FontSize := 8
      else
      if TmpSize > 7 then
        Props.FontSize := 36
      else
        Props.FontSize := FONTSIZESVALUSARRAY[TmpSize-1];
    end;
  end;
  if Color <> -1 then
    Props.FontColor := Color;
end;

constructor TIpHtmlNodeFONT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FSize := TIpHtmlRelSize.Create;
end;

destructor TIpHtmlNodeFONT.Destroy;
begin
  inherited;
  FSize.Free;
end;

procedure TIpHtmlNodeFONT.SetColor(const Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SetFace(const Value: string);
begin
  if Value <> FFace then begin
    FFace := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SizeChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeFontStyle }

procedure TIpHtmlNodeFontStyle.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
  hfsTT :
    Props.FontName := Owner.FixedTypeface;
  hfsI :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hfsB :
    Props.FontStyle := Props.FontStyle + [fsBold];
  hfsU :
    Props.FontStyle := Props.FontStyle + [fsUnderline];
  hfsSTRIKE,
  hfsS :
    Props.FontStyle := Props.FontStyle + [fsStrikeout];
  hfsBIG :
    Props.FontSize := Props.FontSize + 2;
  hfsSMALL :
    Props.FontSize := Props.FontSize - 2;
  hfsSUB :
    begin
      Props.FontSize := Props.FontSize - 4;
      Props.FontBaseline := Props.FontBaseline - 2;
    end;
  hfsSUP :
    begin
      Props.FontSize := Props.FontSize - 4;
      Props.FontBaseline := Props.FontBaseline + 4;
    end;
  end;
  {$IFDEF IP_LAZARUS}
  case Style of
    hfsTT    : FElementName := 'tt';
    hfsI     : FElementName := 'i';
    hfsB     : FElementName := 'b';
    hfsU     : FElementName := 'u';
    hfsSTRIKE: FElementName := 'strike';
    hfsS     : FElementName := 's';
    hfsBIG   : FElementName := 'big';
    hfsSMALL : FElementName := 'small';
    hfsSUB   : FElementName := 'sub';
    hfsSUP   : FElementName := 'sup';
  end;
  {$ENDIF}
end;

{ TIpHtmlNodeBlock }

constructor TIpHtmlNodeBlock.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FElementQueue := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  FBlockMin := -1;
  FBlockMax := -1;
  FBgColor := -1;
  FTextColor := -1;
  FBackground := '';
end;

destructor TIpHtmlNodeBlock.Destroy;
begin
  ClearWordList;
  FElementQueue.Free;
  FElementQueue := nil;
  inherited;
end;

procedure TIpHtmlNodeBlock.SetBackground(const AValue: string);
begin
  if AValue <> FBackground then begin
    FBackground := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.SetBgColor(const AValue: TColor);
begin
  if AValue <> FBgColor then begin
    FBgColor := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.SetTextColor(const AValue: TColor);
begin
  if AValue <> FTextColor then begin
    FTextColor := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.DoRenderFont(var aCurWord: PIpHtmlElement);
begin
  {$IFDEF IP_LAZARUS}
  FCanvas.Font.BeginUpdate; // for speedup
  {$ENDIF}
  if (FCurProps = nil) or not FCurProps.AIsEqualTo(aCurWord.Props) then
    with aCurWord.Props do begin
      FCanvas.Font.Name := FontName;
      if ScaleFonts then
        FCanvas.Font.Size := round(FontSize * Aspect)
      else
        FCanvas.Font.Size := FontSize;
      FCanvas.Font.Style := FontStyle;
    end;
  if ScaleBitmaps and BWPRinter then
    Owner.Target.Font.Color := clBlack
  else
    if (FCurProps = nil) or not FCurProps.BIsEqualTo(aCurWord.Props) then
      FCanvas.Font.Color := aCurWord.Props.FontColor;
  {$IFDEF IP_LAZARUS}
  Owner.Target.Font.EndUpdate;
  {$ENDIF}
  FCurProps := aCurWord.Props;
end;

procedure TIpHtmlNodeBlock.DoRenderElemWord(aCurWord: PIpHtmlElement; aCurTabFocus: TIpHtmlNode);
var
  P : TPoint;
  R : TRect;
  {$IFDEF IP_LAZARUS}
  OldBrushcolor: TColor;
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
  OldBrushStyle: TBrushStyle;

  procedure saveCanvasProperties;
  begin
    OldBrushColor := FCanvas.Brush.Color;
    OldBrushStyle := FCanvas.Brush.Style;
    OldFontColor := FCanvas.Font.Color;
    OldFontStyle := FCanvas.Font.Style;
  end;

  procedure restoreCanvasProperties;
  begin
    FCanvas.Font.Color := OldFontColor;
    FCanvas.Brush.Color := OldBrushColor;
    FCanvas.Brush.Style := OldBrushStyle;
    FCanvas.Font.Style := OldFontStyle;
  end;
  {$ENDIF}

begin
  P := Owner.PagePtToScreen(aCurWord.WordRect2.TopLeft);
  {$IFDEF IP_LAZARUS}
  //if (LastOwner <> aCurWord.Owner) then LastPoint := P;
  saveCanvasProperties;
  if aCurWord.IsSelected or Owner.AllSelected then begin
    FCanvas.Font.color := clHighlightText;
    FCanvas.brush.Style := bsSolid;
    FCanvas.brush.color := clHighLight;
    Owner.PageRectToScreen(aCurWord.WordRect2, R);
    FCanvas.FillRect(R);
  end
  else if FCurProps.BgColor > 0 then
  begin
    FCanvas.brush.Style := bsSolid;
    FCanvas.brush.color := FCurProps.BgColor;
  end
  else
  {$ENDIF}
    FCanvas.Brush.Style := bsClear;
  //debugln(['TIpHtmlNodeBlock.RenderQueue ',aCurWord.AnsiWord]);
  Owner.PageRectToScreen(aCurWord.WordRect2, R);
  {$IFDEF IP_LAZARUS}
  if aCurWord.Owner.FParentNode = aCurTabFocus then
    FCanvas.DrawFocusRect(R);
  if FCanvas.Font.color=-1 then
    FCanvas.Font.color:=clBlack;
  {$ENDIF}
  if aCurWord.AnsiWord <> NAnchorChar then
    FCanvas.TextRect(R, P.x, P.y, NoBreakToSpace(aCurWord.AnsiWord));
  {$IFDEF IP_LAZARUS}
  restoreCanvasProperties;
  {$ENDIF}
  Owner.AddRect(aCurWord.WordRect2, aCurWord, Self);
end;

procedure TIpHtmlNodeBlock.RenderQueue;
var
  CurWord : PIpHtmlElement;
  CurTabFocus: TIpHtmlNode;
  i : Integer;
  R : TRect;
  P : TPoint;
  L0 : Boolean;
begin
  L0 := Level0;
  FCurProps := nil;
  FCanvas := Owner.Target;
  {$IFDEF IP_LAZARUS}
  // to draw focus rect
  i := FOwner.FTabList.Index;
  if (FOwner.FTabList.Count > 0) and (i <> -1) then
    CurTabFocus := TIpHtmlNode(FOwner.FTabList[i])
  else
    CurTabFocus := nil;
  {$ENDIF}
  for i := 0 to Pred(FElementQueue.Count) do begin
    CurWord := PIpHtmlElement(FElementQueue[i]);
    if (CurWord.Props <> nil) and (CurWord.Props <> FCurProps) then
      DoRenderFont(CurWord);

    {$IFDEF IP_LAZARUS_DBG}
    //DumpTIpHtmlProps(FCurProps);
    {$endif}
    //debugln(['TIpHtmlNodeBlock.RenderQueue ',i,' ',IntersectRect(R, CurWord.WordRect2, Owner.PageViewRect),' CurWord.WordRect2=',dbgs(CurWord.WordRect2),' Owner.PageViewRect=',dbgs(Owner.PageViewRect)]);
    if IntersectRect(R, CurWord.WordRect2, Owner.PageViewRect) then
      case CurWord.ElementType of
      etWord :
        DoRenderElemWord(CurWord, CurTabFocus);
      etObject :
        begin
          TIpHtmlNodeAlignInline(CurWord.Owner).Draw(Self);
          //Owner.AddRect(CurWord.WordRect2, CurWord, Self);
          FCurProps := nil;
        end;
      etSoftHyphen :
        begin
          P := Owner.PagePtToScreen(CurWord.WordRect2.TopLeft);
          FCanvas.Brush.Style := bsClear;
          FCanvas.TextOut(P.x, P.y, '-');
          Owner.AddRect(CurWord.WordRect2, CurWord, Self);
        end;
      end
    else
      case CurWord.ElementType of
      etWord,
      etObject,
      etSoftHyphen :
        if (CurWord.WordRect2.Bottom <> 0)
        and (CurWord.WordRect2.Top > Owner.PageViewRect.Bottom)
        and L0 then
          break;
      end;
  end;
end;

procedure TIpHtmlNodeBlock.Render(const RenderProps: TIpHtmlProps);
begin
  if not RenderProps.IsEqualTo(Props) then
  begin
    Props.Assign(RenderProps);
    LoadAndApplyCSSProps;
    SetProps(Props);
  end;
  if FElementQueue.Count = 0 then
    Enqueue;
  RenderQueue;
end;

procedure TIpHtmlNodeBlock.UpdateCurrent(Start: Integer; const CurProps : TIpHtmlProps);
{- update other words that use same properties as the one at Start with their lengths.
  Cuts down on the number of time the font properties need to be changed.}
var
  i : Integer;
  CurElem : PIpHtmlElement;
begin
  for i := FElementQueue.Count - 1 downto Start + 1 do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    if (CurElem.ElementType = etWord) and (CurElem.IsBlank = 0)
    and ( (CurElem.Props = nil) or CurElem.Props.AIsEqualTo(CurProps) )
    and (CurElem.SizeProp <> CurProps.PropA) then begin
      CurElem.Size := Owner.Target.TextExtent(NoBreakToSpace(CurElem.AnsiWord));
      if CurElem.AnsiWord = NAnchorChar then
        CurElem.Size.cx := 1;
      CurElem.SizeProp := CurProps.PropA;
    end;
  end;
end;

procedure TIpHtmlNodeBlock.LoadAndApplyCSSProps;
begin
  inherited LoadAndApplyCSSProps;
  if FCombinedCSSProps <> nil then begin
    if FCombinedCSSProps.Color <> -1 then
      TextColor := FCombinedCSSProps.Color;
    if FCombinedCSSProps.BgColor <> -1 then
      BgColor := FCombinedCSSProps.BGColor;
  end;
end;

procedure TIpHtmlNodeBlock.UpdSpaceHyphenSize(aProps: TIpHtmlProps);
begin
  if aProps.PropA.SizeOfSpaceKnown then begin
    FSizeOfSpace := aProps.PropA.KnownSizeOfSpace;
    FSizeOfHyphen := aProps.PropA.KnownSizeOfHyphen;
  end else begin
    Assert(aProps.PropA.tmHeight = 0, 'UpdSpaceHyphenSize: PropA.tmHeight > 0');
    FCanvas.Font.Name := aProps.FontName;
    FCanvas.Font.Size := aProps.FontSize;
    FCanvas.Font.Style := aProps.FontStyle;
    FSizeOfSpace := FCanvas.TextExtent(' ');
    {$IFDEF IP_LAZARUS_DBG}
    if FSizeOfSpace.CX=0 then
      DebugLn('TIpHtmlNodeBlock.CalcMinMaxQueueWidth Font not found "',FCanvas.Font.Name,'" Size=',dbgs(FCanvas.Font.Size));
    {$ENDIF}
    FSizeOfHyphen := FCanvas.TextExtent('-');
    aProps.PropA.SetKnownSizeOfSpace(FSizeOfSpace);
    aProps.PropA.KnownSizeOfHyphen := FSizeOfHyphen;
  end;
end;

procedure TIpHtmlNodeBlock.UpdPropMetrics(aProps: TIpHtmlProps);
var
  TextMetrics : TLCLTextMetric;  // TTextMetric;
begin             // Debug: remove assertions later
  Assert(aProps.PropA.tmHeight = 0, 'UpdPropMetrics: PropA.tmHeight > 0');
  Assert(FCanvas.Font.Name = aProps.FontName, 'UpdPropMetrics: FCanvas.Font.Name <> aProps.FontName');
  Assert(FCanvas.Font.Size = aProps.FontSize, 'UpdPropMetrics: FCanvas.Font.Size <> aProps.FontSize');
  Assert(FCanvas.Font.Style = aProps.FontStyle, 'UpdPropMetrics: FCanvas.Font.Style <> aProps.FontStyle');
  {$IFDEF IP_LAZARUS}
  FCanvas.GetTextMetrics(TextMetrics);
  aProps.PropA.tmAscent := TextMetrics.Ascender;
  aProps.PropA.tmDescent := TextMetrics.Descender;
  aProps.PropA.tmHeight := TextMetrics.Height;
  {$ELSE}
  GetTextMetrics(FCanvas.Handle, TextMetrics);
  aProps.PropA.tmAscent := TextMetrics.tmAscent;
  aProps.PropA.tmDescent := TextMetrics.tmDescent;
  aProps.PropA.tmHeight := TextMetrics.tmHeight;
  {$ENDIF}
end;

procedure TIpHtmlNodeBlock.CalcMinMaxQueueWidth(var aMin, aMax: Integer);
var
  CurElem : PIpHtmlElement;
  CurProps : TIpHtmlProps;
  CurFontName : string;
  CurFontSize : Integer;
  CurFontStyle : TFontStyles;
  i : Integer;
  MinW, MaxW, IndentW, TextWidth : Integer;
  LIndent, LIndentP : Integer;
  LastW, LastElement : Integer;
  NoBr : Boolean;

  procedure ApplyMinMaxProps;
  var
    Changed : Boolean;
  begin
    if (CurProps = nil) or not CurElem.Props.AIsEqualTo(CurProps) then begin
      Changed := False;
      if (CurProps = nil) or (CurFontName <> CurElem.Props.FontName)
      or (CurFontName = '') then begin
        CurFontName := CurElem.Props.FontName;
        FCanvas.Font.Name := CurFontName;
        Changed := True;
      end;
      if (CurProps = nil) or (CurFontSize <> CurElem.Props.FontSize)
      or (CurFontSize = 0) then begin
        CurFontSize := CurElem.Props.FontSize;
        FCanvas.Font.Size := CurFontSize;
        Changed := True;
      end;
      if (CurProps = nil) or (CurFontStyle <> CurElem.Props.FontStyle) then begin
        CurFontStyle := CurElem.Props.FontStyle;
        FCanvas.Font.Style := CurFontStyle;
        Changed := True;
      end;
      UpdSpaceHyphenSize(CurElem.Props);
      if Changed and (CurElem.Props.PropA.tmHeight = 0) then
        UpdPropMetrics(CurElem.Props);
    end;
  end;

begin
  FCanvas := Owner.Target;
  aMin := 0;
  aMax := 0;
  if FElementQueue.Count = 0 then Exit;
  LIndent := 0;
  LIndentP := 0;
  LastElement := TrimTrailingBlanks;   // Trim trailing blanks
  CurProps := nil;
  CurFontName := '';
  CurFontSize := 0;
  CurFontStyle := [];
  FCanvas.Font.Style := CurFontStyle;
  FSizeOfSpace := FCanvas.TextExtent(' ');
  FSizeOfHyphen := FCanvas.TextExtent('-');
  i := 0;
  NoBr := False;
  while i <= LastElement do begin
    TextWidth := 0;
    IndentW := 0;
    LastW := 0;
    while (i <= LastElement) do begin
      MinW := 0;
      CurElem := PIpHtmlElement(FElementQueue[i]);
      if CurElem.Props <> nil then begin
        ApplyMinMaxProps;
        NoBr := CurElem.Props.NoBreak;
        CurProps := CurElem.Props;
      end;
      case CurElem.ElementType of
      etWord :
        begin
        {determine height and width of word}
        if CurElem.IsBlank <> 0 then begin
          MaxW := FSizeOfSpace.cx * CurElem.IsBlank;
          MinW := MaxW;
          if NoBr then
            MinW := MinW + LastW;
        end else begin
          if (CurElem.SizeProp = CurProps.PropA) then
            MaxW := CurElem.Size.cx
          else begin
            CurElem.Size := FCanvas.TextExtent(NoBreakToSpace(CurElem.AnsiWord));
            if CurElem.AnsiWord = NAnchorChar then
              CurElem.Size.cx := 1;
            MaxW := CurElem.Size.cx;
            CurElem.SizeProp := CurProps.PropA;
            UpdateCurrent(i, CurProps);
          end;
          MinW := MaxW + LastW;
        end;
        LastW := MinW;
        end;
      etObject :
        begin
          TIpHtmlNodeAlignInline(CurElem.Owner).CalcMinMaxWidth(MinW, MaxW);
          LastW := 0;
          CurProps := nil;
        end;
      etSoftLF..etClearBoth :
        begin
          if TextWidth + IndentW > aMax then
            aMax := TextWidth + IndentW;
          TextWidth := 0;
          MinW := 0;
          MaxW := 0;
          Inc(i);
          break;
        end;
      etIndent :
        begin
          Inc(LIndent);
          LIndentP := LIndent * StdIndent;
          if LIndentP > IndentW then
            IndentW := LIndentP;
          MinW := 0;
          MaxW := 0;
        end;
      etOutdent :
        begin
          if LIndent > 0 then begin
            Dec(LIndent);
            LIndentP := LIndent * StdIndent;
          end;
        MinW := 0;
        MaxW := 0;
        end;
      etSoftHyphen :
        begin
          MaxW := FSizeOfHyphen.cx;
          MinW := MaxW + LastW;
        end;
      end;
      Inc(MinW, LIndentP);
      if MinW > aMin then
        aMin := MinW;
      Inc(TextWidth, MaxW);
      Inc(i);
    end;

    aMax := MaxI2(aMax, TextWidth + IndentW);
  end;
end;

procedure TIpHtmlNodeBlock.CalcMinMaxPropWidth(const RenderProps: TIpHtmlProps;
  var aMin, aMax: Integer);
begin
  if RenderProps.IsEqualTo(Props) and (FBlockMin <> -1) and (FBlockMax <> -1) then begin
    aMin := FBlockMin;
    aMax := FBlockMax;
    Exit;
  end;
  Props.Assign(RenderProps);
  LoadAndApplyCSSProps;
  SetProps(Props);
  if FElementQueue.Count = 0 then
    Enqueue;
  CalcMinMaxQueueWidth(aMin, aMax);
  FBlockMin := aMin;
  FBlockMax := aMax;
end;

procedure TIpHtmlNodeBlock.ClearWordList;
begin
  if FElementQueue <> nil then
    FElementQueue.Clear;
end;

procedure TIpHtmlNodeBlock.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FElementQueue.Add(Entry);
end;

procedure TIpHtmlNodeBlock.Invalidate;
var
  R : TRect;
begin
  if PageRectToScreen(PageRect, R) then
    Owner.InvalidateRect(R);
end;

function TIpHtmlNodeBlock.GetHeight(const RenderProps: TIpHtmlProps;
                                    const Width: Integer): Integer;
begin
  if FLastW = Width then begin
    Result := FLastH;
    Exit;
  end;
  Layout(RenderProps, Rect(0, 0, Width, MaxInt));
  Result := PageRect.Bottom;
  FLastH := Result;
  FLastW := Width;
end;

procedure TIpHtmlNodeBlock.Layout(const RenderProps: TIpHtmlProps; const TargetRect: TRect);
begin
  if EqualRect(TargetRect, PageRect) then Exit;
  if not RenderProps.IsEqualTo(Props) then
  begin
    Props.Assign(RenderProps);
    LoadAndApplyCSSProps;
    SetProps(Props);
  end;
  if FElementQueue.Count = 0 then
    Enqueue;
  if SameDimensions(TargetRect, PageRect) then
    RelocateQueue(TargetRect.Left - PageRect.Left, TargetRect.Top - PageRect.Top)
  else
    LayoutQueue(TargetRect);
end;

procedure TIpHtmlNodeBlock.RelocateQueue(const dx, dy: Integer);
var
  i : Integer;
  CurElem : PIpHtmlElement;
  R : TRect;
begin
  OffsetRect(FPageRect, dx, dy);
  for i := 0 to Pred(FElementQueue.Count) do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    R := CurElem.WordRect2;
    if R.Bottom <> 0 then begin
      OffsetRect(R, dx, dy);
      SetWordRect(CurElem, R);
    end;
  end;
end;

procedure TIpHtmlNodeBlock.QueueInit(const TargetRect: TRect);
begin
  FWordInfoSize := 0;
  FWordInfo := nil;
  YYY := TargetRect.Top;
  FLeftQueue := TFPList.Create;
  FRightQueue := TFPList.Create;
  //FSizeOfSpace := Owner.Target.TextExtent(' ');
  //FSizeOfHyphen := Owner.Target.TextExtent('-');
  FCurProps := nil;
  FLIdent := 0;
  FRIdent := 0;
  FVRemainL := 0;
  FVRemainR := 0;
  FClear := cNone;
  FExpBreak := True;
  FTempCenter := False;
  FSaveAl := haLeft;
  FIgnoreHardLF := False;
  FLastBreakpoint := 0;
  FPageRect := TargetRect;
  FMaxHeight := 0;
  FMaxAscent := 0;
  FMaxDescent := 0;
  FLineBreak := False;
  FAl := haLeft;
  FVAL := hva3Top;
  FCurAscent := 0;
  FCurDescent := 0;
  FCurHeight := 0;
end;

procedure TIpHtmlNodeBlock.InitMetrics;
{$IFDEF IP_LAZARUS}
var
  TextMetrics : TLCLTextMetric;
begin
  FCanvas.GetTextMetrics(TextMetrics);
  FBlockAscent := TextMetrics.Ascender;
  FBlockDescent := TextMetrics.Descender;
  FBlockHeight := TextMetrics.Height;
end;
{$ELSE}
var
  TextMetrics : TTextMetric;
begin
  GetTextMetrics(aCanvas.Handle, TextMetrics);
  BlockAscent := TextMetrics.tmAscent;
  BlockDescent := TextMetrics.tmDescent;
  BlockHeight := TextMetrics.tmHeight;
end;
{$ENDIF}

function TIpHtmlNodeBlock.QueueLeadingObjects: Integer;
// Returns the first element index.
var
  CurObj : TIpHtmlNodeAlignInline;
  CurElem : PIpHtmlElement;
begin
  Result := 0;
  while Result <= FElementQueue.Count-1 do begin
    CurElem := PIpHtmlElement(FElementQueue[Result]);
    case CurElem.ElementType of
      etObject :
        begin
          CurObj := TIpHtmlNodeAlignInline(CurElem.Owner);
          case CurObj.Align of
          hiaLeft :
            begin
              FLeftQueue.Add(CurElem);
              Inc(Result);
            end;
          hiaRight :
            begin
              FRightQueue.Add(CurElem);
              Inc(Result);
            end;
          else
            break;
          end;
        end
      else
        break;
    end;
  end;
end;

function TIpHtmlNodeBlock.TrimTrailingBlanks(aFirstElem: Integer): Integer;
// Trim trailing blanks. Returns the last element index.
var
  CurElem: PIpHtmlElement;
begin
  Result := FElementQueue.Count - 1;
  repeat
    if (Result < aFirstElem) then Break;
    CurElem := PIpHtmlElement(FElementQueue[Result]);
    if (CurElem.ElementType <> etWord) or (CurElem.IsBlank = 0) then Break;
    Dec(Result)
  until false;
end;

procedure TIpHtmlNodeBlock.DoQueueAlign(const TargetRect: TRect; aExpLIndent: Integer);

  procedure DoQueueAlignSub(aQueue: TFPList; aRight: Boolean);
  var
    CurElem: PIpHtmlElement;
    CurObj: TIpHtmlNodeAlignInline;
    xLeft, xRight, ySize: Integer;
    RectWidth: Integer;
  begin
    if aRight then
      xLeft := FVRemainR
    else
      xLeft := FVRemainL;
    if (aQueue.Count > 0) and (xLeft = 0) then
      while aQueue.Count > 0 do begin
        CurElem := aQueue[0];
        CurObj := TIpHtmlNodeAlignInline(CurElem.Owner);
        RectWidth := TargetRect.Right - TargetRect.Left;
        FxySize := CurObj.GetDim(RectWidth);
        FTotWidth := RectWidth - FLIdent - FRIdent - FxySize.cx - aExpLIndent;
        if FTotWidth < 0 then
          break;
        if aRight then begin
          xRight := TargetRect.Right - FRIdent;
          xLeft := xRight - FxySize.cx;
          Inc(FRIdent, FxySize.cx);
          FVRemainR := MaxI2(FVRemainR, FxySize.cy)
        end
        else begin
          xLeft := TargetRect.Left + FLIdent;
          xRight := xLeft + FxySize.cx;
          Inc(FLIdent, FxySize.cx);
          FVRemainL := MaxI2(FVRemainL, FxySize.cy);
        end;
        ySize := FxySize.cy;
        SetWordRect(CurElem, Rect(xLeft, YYY, xRight, YYY+FxySize.cy));
        Assert(ySize = FxySize.cy, 'TIpHtmlNodeBlock.DoQueueAligned: ySize <> FSize.cy'); // Can be removed later.
        aQueue.Delete(0);
      end;
  end;

begin
  DoQueueAlignSub(FLeftQueue, False); // Left
  DoQueueAlignSub(FRightQueue, True); // Right
end;

procedure TIpHtmlNodeBlock.OutputQueueLine;
var
  WDelta, WMod : Integer;

  function CalcDelta: Integer;     // Returns dx
  var
    m : Integer;
  begin
    WDelta := 0;
    WMod := 0;
    Result := 0;
    case FAl of
      haUnknown :  // by Juha
        Assert(False, 'TIpHtmlNodeBlock.OutputQueueLine: Align = Unknown.');
      haDefault, haLeft :
        ;
      haCenter :
        if FTotWidth >= FTextWidth then
          Result := (FTotWidth - FTextWidth) div 2;
      haRight :
        if FTotWidth >= FTextWidth then
          Result := FTotWidth - FTextWidth;
      haChar :
        if FTotWidth >= FTextWidth then
          Result := (FTotWidth - FTextWidth) div 2;
      else //haJustify :
        if iElem < FElementQueue.Count then begin
          m := iElem - FFirstWord - 2;
          if m > 0 then begin
            WDelta := (FTotWidth - FTextWidth) div m;
            WMod := (FTotWidth - FTextWidth) mod m;
          end;
        end;
    end;
  end;

var
  j, dx : Integer;
  R : TRect;
  CurElem : PIpHtmlElement;
  CurWordInfo : PWordInfo;
begin
  dx := CalcDelta;
  if Owner.PageHeight <> 0 then begin
    {if we're printing, adjust line's vertical offset to not straddle a page boundary}
    j := YYY mod Owner.PageHeight;
    {only do this for 'small' objects, like text lines}
    if (FMaxAscent + FMaxDescent < 200)
    and (j + FMaxAscent + FMaxDescent > Owner.PageHeight) then
      Inc(YYY, ((j + FMaxAscent + FMaxDescent) - Owner.PageHeight));
  end;

  for j := FFirstWord to FLastWord do begin
    CurElem := PIpHtmlElement(FElementQueue[j]);
    CurWordInfo := @FWordInfo[j - FFirstWord];
    if CurWordInfo.Sz.cx <> 0 then begin
      R.Left := CurWordInfo.BaseX;
      R.Right := R.Left + CurWordInfo.Sz.cx;
      case CurWordInfo.VA of
      hva3Top :
        begin
          R.Top := YYY;
          R.Bottom := YYY + CurWordInfo.Sz.cy;
        end;
      hva3Middle :
        begin
          R.Top := YYY + (FMaxHeight - CurWordInfo.Sz.cy) div 2;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      hva3Bottom :
        begin
          R.Top := YYY + FMaxHeight - CurWordInfo.Sz.cy;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      hva3Default,
      hva3Baseline :
        begin
          if CurWordInfo.CurAsc >= 0 then
            R.Top := YYY + FMaxAscent - CurWordInfo.CurAsc
          else
            R.Top := YYY;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      end;
      if WMod <> 0 then begin
        OffsetRect(R, dx + WDelta + 1, 0);
        Dec(WMod);
      end else
        OffsetRect(R, dx + WDelta, 0);
      SetWordRect(CurElem, R);
    end else
      SetWordRect(CurElem, NullRect);
  end;

  if FTempCenter then begin
    FAl := FSaveAl;
    FTempCenter := False;
  end;
end;

procedure TIpHtmlNodeBlock.DoQueueClear;
begin
  case FClear of
    cLeft :
      if FVRemainL > 0 then begin
        Inc(YYY, FVRemainL);
        FVRemainL := 0;
        FLIdent := 0;
      end;
    cRight :
      if FVRemainR > 0 then begin
        Inc(YYY, FVRemainR);
        FVRemainR := 0;
        FRIdent := 0;
      end;
    cBoth :
      begin
        Inc(YYY, MaxI2(FVRemainL, FVRemainR));
        FVRemainL := 0;
        FVRemainR := 0;
        FLIdent := 0;
        FRIdent := 0;
      end;
  end;
  FClear := cNone;
end;

procedure TIpHtmlNodeBlock.ApplyQueueProps(aCurElem: PIpHtmlElement; var aPrefor: Boolean);
var
  TextMetrics : TLCLTextMetric;
begin
  with aCurElem.Props do begin
    if (FCurProps = nil) or not AIsEqualTo(FCurProps) then begin
      UpdSpaceHyphenSize(aCurElem.Props);
      if PropA.tmHeight = 0 then
        UpdPropMetrics(aCurElem.Props);
      FBlockHeight := PropA.tmHeight;
      FBlockAscent := PropA.tmAscent;
      FBlockDescent := PropA.tmDescent;
    end;
    if (FCurProps = nil) or not BIsEqualTo(FCurProps) then begin
      FAl := Alignment;
      FVAL := VAlignment;
      FBaseOffset := FontBaseline;
      aPrefor := Preformatted;
    end;
  end;
  FCurProps := aCurElem.Props;
end;

procedure TIpHtmlNodeBlock.DoQueueElemWord(aCurElem: PIpHtmlElement);
begin
  FIgnoreHardLF := False;
  if FLTrim and (aCurElem.IsBlank <> 0) then
    FxySize := SizeRec(0, 0)
  else begin
    if aCurElem.IsBlank <> 0 then begin
      FxySize.cx := FSizeOfSpace.cx * aCurElem.IsBlank;
      FxySize.cy := FSizeOfSpace.cy;
      FCanBreak := True;
    end else begin
      if (aCurElem.SizeProp = FCurProps.PropA) then
        FxySize := aCurElem.Size
      else begin
        FCanvas.Font.Name := FCurProps.FontName;
        FCanvas.Font.Size := FCurProps.FontSize;
        FCanvas.Font.Style := FCurProps.FontStyle;
        aCurElem.Size := FCanvas.TextExtent(NoBreakToSpace(aCurElem.AnsiWord));
        FxySize := aCurElem.Size;
        aCurElem.SizeProp := FCurProps.PropA;
      end;
    end;
    FLTrim := False;
    FLineBreak := False;
    FExpBreak := False;
  end;
  FCurAscent := FBlockAscent;
  FCurDescent := FBlockDescent;
  FCurHeight := FBlockHeight;
end;

function TIpHtmlNodeBlock.DoQueueElemObject(var aCurElem: PIpHtmlElement): boolean;

  procedure ObjectVertical(Ascent, Descent: Integer);
  begin
    FExpBreak := False;
    FLTrim := False;
    FCurAscent := Ascent;
    FCurDescent := Descent;
  end;

  function ObjectHorizontal: boolean;
  begin
    aCurElem := nil;
    FCurHeight := 0;
    FxySize.cx := 0;
    Result := FLTrim;
    if Result then
      Inc(iElem);
  end;

var
  CurObj : TIpHtmlNodeAlignInline;
begin
  FIgnoreHardLF := False;
  FCurAscent := 0;
  FCurDescent := 0;
  FCanBreak := True;
  FLineBreak := False;
  CurObj := TIpHtmlNodeAlignInline(aCurElem.Owner);
  FxySize := CurObj.GetDim(FTotWidth);
  FCurHeight := FxySize.cy;
  case Curobj.Align of
  hiaCenter : begin
      ObjectVertical(FMaxAscent, FxySize.cy - FMaxAscent);
      FTempCenter := True;
      FSaveAl := FAl;
      FAl := haCenter;
    end;
  hiaTop :
      ObjectVertical(-1, FxySize.cy);
  hiaMiddle :
      ObjectVertical(FxySize.cy div 2, FxySize.cy div 2);
  hiaBottom :
      ObjectVertical(FxySize.cy, 0);
  hiaLeft : begin
      FLeftQueue.Add(aCurElem);
      if ObjectHorizontal then
        Exit(False);
    end;
  hiaRight : begin
      FRightQueue.Add(aCurElem);
      if ObjectHorizontal then
        Exit(False);
    end;
  end;
  Result := True;
end;

function TIpHtmlNodeBlock.DoQueueElemSoftLF(const W: Integer): boolean;
// Returns FIgnoreHardLF
var
  PendingLineBreak : Boolean;
begin
  if FLineBreak or FExpBreak then begin
    FMaxAscent := 0;
    FMaxDescent := 0;
    PendingLineBreak := False;
  end else begin
    if FMaxAscent = 0 then begin
      FMaxAscent := MaxI2(FMaxAscent, FBlockAscent);
      FMaxDescent := MaxI2(FMaxDescent, FBlockDescent);
    end;
    PendingLineBreak := True;
  end;
  FExpBreak := True;
  if FLineBreak then
    FMaxDescent := 0;
  Inc(iElem);
  FLastWord := iElem - 2;
  if PendingLineBreak then
    FLineBreak := True;
  Result := FIgnoreHardLF;
  if Result then begin
    FxySize.cx := W + 1;
    FSoftLF := True;
  end;
end;

function TIpHtmlNodeBlock.DoQueueElemHardLF: boolean;
// Returns FIgnoreHardLF
begin
  FExpBreak := True;
  if FMaxAscent = 0 then begin
    FMaxAscent := MaxI2(FMaxAscent, FBlockAscent);
    FMaxDescent := MaxI2(FMaxDescent, FBlockDescent);
  end;
  if FLineBreak then
    FMaxDescent := 0;
  FLastWord := iElem - 1;
  Result := FIgnoreHardLF;
  if not Result then begin
    if FLineBreak then  begin
      FMaxAscent := Round (FMaxAscent * Owner.FactBAParag);
      FMaxDescent := Round (FMaxDescent * Owner.FactBAParag);
    end;
    Inc(iElem);
  end;
end;

function TIpHtmlNodeBlock.DoQueueElemClear(aCurElem: PIpHtmlElement): boolean;
// Returns FIgnoreHardLF
begin
  FExpBreak := True;
  case aCurElem.ElementType of
    etClearLeft : FClear := cLeft;
    etClearRight : FClear := cRight;
    etClearBoth : FClear := cBoth;
  end;
  if FLineBreak then
    FMaxDescent := 0;
  Inc(iElem);
  FLastWord := iElem - 2;
  Result := FIgnoreHardLF;
end;

procedure TIpHtmlNodeBlock.DoQueueElemIndentOutdent;
begin
  FCurAscent := 1;
  FCurDescent := 0;
  FCurHeight := 1;
  FxySize := SizeRec(0, 0);
  FCanBreak := True;
end;

procedure TIpHtmlNodeBlock.DoQueueElemSoftHyphen;
begin
  FIgnoreHardLF := False;
  FxySize := FSizeOfHyphen;
  FxySize.cy := FSizeOfSpace.cy;
  FHyphenSpace := FxySize.cx;
  FCanBreak := True;
  FLTrim := False;
  FLineBreak := False;
  FExpBreak := False;
  FCurAscent := FBlockAscent;
  FCurDescent := FBlockDescent;
  FCurHeight := FBlockHeight;
end;

function TIpHtmlNodeBlock.CalcVRemain(aVRemain: integer; var aIdent: integer): integer;
begin
  if aVRemain > 0 then begin
    if FSoftBreak and (FTextWidth = 0) and (FMaxAscent + FMaxDescent = 0) then begin
      Inc(YYY, aVRemain);
      aVRemain := 0;
      aIdent := 0;
    end else begin
      Dec(aVRemain, FMaxAscent + FMaxDescent);
      if aVRemain <= 0 then begin
        aVRemain := 0;
        aIdent := 0;
      end;
    end;
  end;
  Result := aVRemain;
end;

procedure TIpHtmlNodeBlock.SetWordInfoLength(NewLength : Integer);
var
  NewWordInfoSize: Integer;
  {$IFNDEF IP_LAZARUS}
  NewWordInfo: PWordList;
  {$ENDIF}
begin
  if (FWordInfo = nil) or (NewLength > FWordInfoSize) then begin
    NewWordInfoSize := ((NewLength div 256) + 1) * 256;
    {$IFDEF IP_LAZARUS code below does not check if FWordInfo<>nil}
    ReallocMem(FWordInfo,NewWordInfoSize * sizeof(TWordInfo));
    {$ELSE}
    NewWordInfo := AllocMem(NewWordInfoSize * sizeof(TWordInfo));
    move(WordInfo^, NewWordInfo^, WordInfoSize);
    Freemem(WordInfo);
    WordInfo := NewWordInfo;
    {$ENDIF}
    FWordInfoSize := NewWordInfoSize;
  end;
end;

function TIpHtmlNodeBlock.NextElemIsSoftLF: Boolean;
var
  NextElem: PIpHtmlElement;
begin
  Result := False;
  if iElem < FElementQueue.Count-1 then begin
    NextElem := PIpHtmlElement(FElementQueue[iElem+1]);
    Result := NextElem.ElementType = etSoftLF;
  end;
end;

{$IFDEF IP_LAZARUS_DBG}
procedure TIpHtmlNodeBlock.DumpQueue(bStart: boolean=true);
var
  i: Integer;
  CurElem : PIpHtmlElement;
begin
  if bStart then WriteLn('<<<<<')
  else WriteLn('>>>>>');
  for i := 0 to FElementQueue.Count - 1 do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    if CurElem.Owner <> nil then
       write(CurElem.Owner.ClassName,':');
    with CurElem.WordRect2 do
      write(Left,':', Top,':', Right,':', Bottom,':');
    case CurElem.ElementType of
    etWord :
      Write(' wrd:', CurElem.AnsiWord);
    etObject :
      Write(' obj');
    etSoftLF :
      Write(' softlf');
    etHardLF :
      Write(' hardlf');
    etClearLeft :
      Write(' clearleft');
    etClearRight :
      Write(' clearright');
    etClearBoth :
      Write(' clearboth');
    etIndent :
      Write(' indent');
    etOutdent :
      Write(' outdent');
    etSoftHyphen :
      Write(' softhyphen');
    end;
    WriteLn;
  end;
  if bStart then WriteLn('<<<<<')
  else WriteLn('>>>>>');
end;
{$ENDIF}

procedure TIpHtmlNodeBlock.LayoutQueue(const TargetRect: TRect);
var
  WW, X0, ExpLIndent, RectWidth : Integer;
  FirstElem, LastElem : Integer;
  PendingIndent, PendingOutdent : Integer;
  Prefor : Boolean;
  CurElem : PIpHtmlElement;
  wi: PWordInfo;

  procedure InitInner;
  begin
    if PendingIndent > PendingOutdent then begin
      if ExpLIndent < RectWidth - FLIdent - FRIdent then
        Inc(ExpLIndent, (PendingIndent - PendingOutdent) * StdIndent);
    end
    else if PendingOutdent > PendingIndent then begin
      Dec(ExpLIndent, (PendingOutdent - PendingIndent) * StdIndent);
      if ExpLIndent < 0 then
        ExpLIndent := 0;
    end;
    PendingIndent := 0;
    PendingOutdent := 0;
    DoQueueAlign(TargetRect, ExpLIndent);
    FTotWidth := RectWidth - FLIdent - FRIdent - ExpLIndent;
    FLTrim := FLineBreak or (FExpBreak and not Prefor) or (ExpLIndent > 0);
    WW := FTotWidth; // total width we have
    X0 := TargetRect.Left + FLIdent + ExpLIndent;
    FTextWidth := 0;
    FFirstWord := iElem;
    FLastWord := iElem-1;
    FBaseOffset := 0;
    FSoftBreak := False;
    FHyphenSpace := 0;
  end;

  procedure ContinueRow;
  var
    i: Integer;
  begin
    if FCanBreak then
      FLastBreakpoint := iElem;
    FMaxAscent := MaxI2(FMaxAscent, FCurAscent);
    FMaxDescent := MaxI2(FMaxDescent, FCurDescent);
    FMaxHeight := MaxI3(FMaxHeight, FCurHeight, FMaxAscent + FMaxDescent);
    // if word fits on line update width and height
    if CurElem.ElementType = etIndent then begin
      i := StdIndent;
      FxySize.cx := MinI2(WW, i - ((X0 - TargetRect.Left) mod i));
    end;
    Dec(WW, FxySize.cx);
    Inc(FTextWidth, FxySize.cx);
    if FHyphenSpace > 0 then
      for i := 0 to iElem - FFirstWord - 1 do begin
        Assert(i < FWordInfoSize);
        wi := @FWordInfo[i];
        if wi^.Hs > 0 then begin
          Inc(WW, wi^.Hs);
          Dec(FTextWidth, wi^.Hs);
          Dec(X0, wi^.Hs);
          wi^.Hs := 0;
          wi^.Sz.cx := 0;
        end;
      end;
    SetWordInfoLength(iElem - FFirstWord + 1);
    wi := @FWordInfo[iElem - FFirstWord];
    wi^.Sz := SizeRec(FxySize.cx, FCurHeight);
    wi^.BaseX := X0;
    wi^.BOff := FBaseOffset;
    wi^.CurAsc := FCurAscent + FBaseOffset;
    wi^.VA := FVAL;
    wi^.Hs := FHyphenSpace;
    FHyphenSpace := 0;
    Inc(X0, FxySize.cx);
    FLastWord := iElem;
  end;

  procedure EndRow;
  var
    i: Integer;
  begin
    if FHyphenSpace > 0 then
      for i := 0 to iElem - FFirstWord - 2 do begin
        wi := @FWordInfo[i];
        if wi^.Hs > 0 then begin
          Dec(FTextWidth, wi^.Hs);
          wi^.Hs := 0;
          wi^.Sz.cx := 0;
        end;
      end;
    if FCanBreak then
      FLastBreakpoint := iElem - 1;
    if (FLastWord >= 0) and (FLastWord < FElementQueue.Count) then begin
      CurElem := PIpHtmlElement(FElementQueue[FLastWord]);
      if (CurElem.ElementType = etWord)
      and (CurElem.IsBlank <> 0) then begin
        FWordInfo[FLastWord - FFirstWord].Sz.cx := 0;
        FLastWord := iElem - 2;
      end;
    end;
    FLineBreak := True;
    FSoftBreak := not FSoftLF;
  end;

begin
  FCanvas := Owner.Target;
  if FElementQueue.Count = 0 then Exit;
  {$IFDEF IP_LAZARUS_DBG}
  DumpQueue; {debug}
  {$endif}
  try
    QueueInit(TargetRect);
    InitMetrics;
    FirstElem := QueueLeadingObjects;
    LastElem := TrimTrailingBlanks(FirstElem);
    DoQueueAlign(TargetRect, 0);
    Prefor := False;
    ExpLIndent := 0;
    PendingIndent := 0;
    PendingOutdent := 0;
    RectWidth := TargetRect.Right - TargetRect.Left;
    iElem := FirstElem;
    while iElem <= LastElem do begin
      InitInner;
      while iElem < FElementQueue.Count do begin
        FCanBreak := False;
        CurElem := PIpHtmlElement(FElementQueue[iElem]);
        if CurElem.Props <> nil then
          ApplyQueueProps(CurElem, Prefor);
        FSoftLF := False;
        case CurElem.ElementType of
          etWord :
            DoQueueElemWord(CurElem);
          etObject :
            if not DoQueueElemObject(CurElem) then
              Break;
          etSoftLF :
            if not DoQueueElemSoftLF(WW) then
              Break;
          etHardLF :
            if DoQueueElemHardLF then
              raise EIpHtmlException.Create('TIpHtmlNodeBlock.LayoutQueue: FIgnoreHardLF is True after all.')
            else
              Break;
          etClearLeft, etClearRight, etClearBoth :
            if not DoQueueElemClear(CurElem) then
              Break;
          etIndent : begin
              DoQueueElemIndentOutdent;
              if not NextElemIsSoftLF then
                FIgnoreHardLF := True;
              Inc(PendingIndent);
              FLTrim := True;
            end;
          etOutdent : begin
              DoQueueElemIndentOutdent;
              FIgnoreHardLF := False;
              Inc(PendingOutdent);
            end;
          etSoftHyphen :
            DoQueueElemSoftHyphen;
        end;
        FCanBreak := FCanBreak and Assigned(FCurProps) and not FCurProps.NoBreak;
        if (FxySize.cx <= WW) then begin
          ContinueRow;
          Inc(iElem);
        end
        else begin
          EndRow;
          Break;
        end;
      end;

      if FSoftBreak and (FLastBreakpoint > 0) then begin
        FLastWord := FLastBreakpoint;
        iElem := FLastBreakpoint + 1;
      end;
      OutputQueueLine;
      if (not FExpBreak) and (FTextWidth=0) and (FVRemainL=0) and (FVRemainR=0) then
        break;
      Inc(YYY, FMaxAscent + FMaxDescent);

      // Calculate VRemainL and VRemainR
      FVRemainL := CalcVRemain(FVRemainL, FLIdent);
      FVRemainR := CalcVRemain(FVRemainR, FRIdent);
      FMaxHeight := 0;
      FMaxAscent := 0;
      FMaxDescent := 0;
      // prepare for next line
      DoQueueClear;
    end;
    Inc(YYY, MaxI3(FMaxAscent div 2 + FMaxDescent, FVRemainL, FVRemainR));
    FVRemainL := 0;
    FVRemainR := 0;
    FLIdent := 0;
    FRIdent := 0;
    FMaxDescent := 0;

    DoQueueAlign(TargetRect, ExpLIndent);
    Inc(YYY, MaxI3(FMaxAscent + FMaxDescent, FVRemainL, FVRemainR));
    FPageRect.Bottom := YYY;
    {clean up}
  finally
    FLeftQueue.Free;
    FRightQueue.Free;
    if FWordInfo <> nil then
      FreeMem(FWordInfo);
    {$IFDEF IP_LAZARUS_DBG}
    DumpQueue(false); {debug}
    {$endif}
  end;
end;

procedure TIpHtmlNodeBlock.InvalidateSize;
begin
  FBlockMin := -1;
  FBlockMax := -1;
  FLastW := 0;
  FLastH := 0;
  inherited;
end;

function TIpHtmlNodeBlock.Level0: Boolean;
var
  P : TIpHtmlNode;
begin
  Result := True;
  P := FParentNode;
  while P <> nil do begin
    if P is TIpHtmlNodeBlock then begin
      Result := False;
      break;
    end;
    P := P.FParentNode;
  end;
end;

procedure TIpHtmlNodeBlock.ReportCurDrawRects(aOwner: TIpHtmlNode; M : TRectMethod);
var
  i : Integer;
  CurElem : PIpHtmlElement;
begin
  for i := 0 to Pred(FElementQueue.Count) do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    if CurElem.Owner = aOwner then
      M(CurElem.WordRect2);
  end;
end;

function TIpHtmlNodeBlock.CheckSelection(aSelIndex: Integer): Boolean;
var
  CurElem : PIpHtmlElement;
  R : TRect;
begin
  CurElem := PIpHtmlElement(FElementQueue[aSelIndex]);
  R := CurElem.WordRect2;
  if (R.Bottom <> 0) and (R.Top > Owner.FStartSel.Y)
  and (R.Bottom < Owner.FEndSel.Y) then
    Exit(False)
  else
  if PtInRect(R, Owner.FStartSel) or PtInRect(R, Owner.FEndSel) then
    Exit(False)
  else
  if (R.Bottom >= Owner.FStartSel.Y) and (R.Top <= Owner.FEndSel.Y)
  and (R.Left >= Owner.FStartSel.X) and (R.Right <= Owner.FEndSel.X) then
    Exit(False);
  Result := True;
end;

procedure TIpHtmlNodeBlock.AppendSelection(var S: string);
var
  LastY, StartSelIndex, EndSelIndex, i : Integer;
  CurElem : PIpHtmlElement;
  R : TRect;
  LFDone : Boolean;
begin
  if not Owner.AllSelected then begin
    StartSelIndex := 0;
    while StartSelIndex < FElementQueue.Count do begin
      if not CheckSelection(StartSelIndex) then
        Break;
      Inc(StartSelIndex);
    end;
    EndSelIndex := Pred(FElementQueue.Count);
    while EndSelIndex >= 0 do begin
      if not CheckSelection(EndSelIndex) then
        Break;
      Dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := FElementQueue.Count - 1;
  end;
  LastY := -1;
  LFDone := True;
  for i := StartSelIndex to EndSelIndex do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    R := CurElem.WordRect2;
    if not LFDone and (R.Top <> LastY) then begin
      S := S + #13#10;
      LFDone := True;
    end;
    case CurElem.ElementType of
    etWord :
      begin
        S := S + NoBreakToSpace(CurElem.AnsiWord);
        LFDone := False;
      end;
    etObject :
      begin
        TIpHtmlNodeAlignInline(CurElem.Owner).AppendSelection(S);
        LFDone := False;
      end;
    etSoftLF..etClearBoth :
      if not LFDone then begin
        S := S + #13#10;
        LFDone := True;
      end;
    end;
    LastY := R.Top;
  end;
end;

function TIpHtmlNodeBlock.ElementQueueIsEmpty: Boolean;
begin
  Result := FElementQueue.Count = 0;
end;

{ TIpHtmlNodeP }

constructor TIpHtmlNodeP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'p';
  {$ENDIF}
end;

destructor TIpHtmlNodeP.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeP.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeP.Enqueue;
begin
  if FChildren.Count > 0 then begin
    if not (FParentNode is TIpHtmlNodeLI) then begin
      EnqueueElement(Owner.SoftLF);
      EnqueueElement(Owner.HardLF);
    end;
  end;
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
    EnqueueElement(Owner.HardLF);
  end;
end;

procedure TIpHtmlNodeP.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeOBJECT }

destructor TIpHtmlNodeOBJECT.Destroy;
begin
  inherited;
  FWidth.Free;
end;

procedure TIpHtmlNodeOBJECT.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeOL }

procedure TIpHtmlNodeOL.Enqueue;
var
  i : Integer;
begin
  {render list}
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
  end;
  FParentNode.EnqueueElement(Owner.FLIndent);
  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeLI then begin
      Counter := i + 1;
      TIpHtmlNodeLI(FChildren[i]).Enqueue;
      FParentNode.EnqueueElement(Owner.SoftLF);
    end else
      TIpHtmlNode(FChildren[i]).Enqueue;
  FParentNode.EnqueueElement(Owner.FLOutdent);
  FParentNode.EnqueueElement(Owner.SoftLF);
end;

function TIpHtmlNodeOL.GetNumString: string;

  function IntToRomanStr(i : Integer): string;
  const
    RC : array[0..6] of AnsiChar = ('M', 'D', 'C', 'L', 'X', 'V', 'I');
    RV : array[0..6] of Integer = (1000, 500, 100, 50, 10, 5, 1);
  var
    n : Integer;
  begin
    Result := '';
    n := 0;
    repeat
      while i >= RV[n] do begin
        Result := Result + RC[n];
        Dec(i, RV[n]);
      end;
      Inc(n);
    until i = 0;
  end;

begin
  Result := ''; // stop warning
  case Style of
  olArabic :
    str(Counter, Result);
  olLowerAlpha :
    Result := chr(ord('a') + Counter - 1);
  olUpperAlpha :
    Result := chr(ord('A') + Counter - 1);
  olLowerRoman :
    Result := LowerCase(IntToRomanStr(Counter));
  olUpperRoman :
    Result := IntToRomanStr(Counter);
  end;
end;

procedure TIpHtmlNodeOL.SetStart(const Value: Integer);
begin
  if Value <> FStart then begin
    FStart := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeOL.SetOLStyle(const Value: TIpHtmlOLStyle);
begin
  if Value <> FOLStyle then begin
    FOLStyle := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeList }

procedure TIpHtmlNodeList.Enqueue;
var
  i : Integer;
begin
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
  end;
  {render list}
  FParentNode.EnqueueElement(Owner.FLIndent);
  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeLI then begin
      TIpHtmlNodeLI(FChildren[i]).Enqueue;
      FParentNode.EnqueueElement(Owner.SoftLF);
    end else
      TIpHtmlNode(FChildren[i]).Enqueue;
  FParentNode.EnqueueElement(Owner.FLOutdent);
  EnqueueElement(Owner.SoftLF);
end;

procedure TIpHtmlNodeList.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeHeader }

constructor TIpHtmlNodeHeader.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
end;

destructor TIpHtmlNodeHeader.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeHeader.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.FontSize := FONTSIZESVALUSARRAY[abs(Size-6)];
  Props.FontStyle := [fsBold];
  Props.Alignment := Align;
  Props.DelayCache:=False;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeHeader.Enqueue;
begin
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
    EnqueueElement(Owner.HardLF);
  end;
end;

{ TIpHtmlNodeLI }

procedure TIpHtmlNodeLI.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if ScaleBitmaps then begin
    Min := round(8 * Aspect);
    Max := round(8 * Aspect);
  end else begin
    Min := 8;
    Max := 8;
  end;
end;

constructor TIpHtmlNodeLI.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'li';
  {$ENDIF}
  Align := hiaBottom;
  WordEntry := Owner.NewElement(etWord, Self);
  WordEntry.Props := Props;
end;

procedure TIpHtmlNodeLI.Draw;
var
  R : TRect;
  SaveColor : Tcolor;
begin
  if PageRectToScreen(GrossDrawRect, R) then
    case ListType of
    ulDisc :
      begin
        SaveColor := Owner.Target.Brush.Color;
        Owner.Target.Brush.Color := Props.FontColor;
        if ScaleBitmaps then
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
        Owner.Target.Brush.Color := SaveColor;
      end;
    ulSquare :
      begin
        if ScaleBitmaps then
          Owner.Target.Rectangle(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Rectangle(R.Left, R.Top, R.Left + 7, R.Top + 7);
      end;
    ulCircle :
      begin
        if ScaleBitmaps then
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
      end;
    end;
end;

procedure TIpHtmlNodeLI.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeLI.Enqueue;
var
  S : string;
  i : Integer;
begin
  if FParentNode is TIpHtmlNodeOL then begin
    S := TIpHtmlNodeOL(FParentNode).GetNumString;
    SetRawWordValue(WordEntry, S + '.');
    EnqueueElement(WordEntry);
  end else
    EnqueueElement(Element);
  EnqueueElement(Owner.FLIndent);
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).Enqueue;
  EnqueueElement(Owner.FLOutdent);
end;

function TIpHtmlNodeLI.GetDim(ParentWidth: Integer): TSize;
begin
  if ScaleBitmaps then
    Result := SizeRec(round(Aspect * 8), round(Aspect * 8))
  else
    Result := SizeRec(8, 8);
end;

function TIpHtmlNodeLI.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeLI.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeLI.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeBR }

procedure TIpHtmlNodeBR.Enqueue;
begin
  case Clear of
  hbcNone :
    EnqueueElement(Owner.HardLF);
  hbcLeft :
    EnqueueElement(Owner.HardLFClearLeft);
  hbcRight :
    EnqueueElement(Owner.HardLFClearRight);
  hbcAll :
    EnqueueElement(Owner.HardLFClearBoth);
  end;
end;

procedure TIpHtmlNodeBR.SetClear(const Value: TIpHtmlBreakClear);
begin
  FClear := Value;
  InvalidateSize;
end;

constructor TIpHtmlNodeBR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$ifdef IP_LAZARUS}
  FElementName := 'br';
  {$endif}
end;

{ TIpHtmlNodeHR }

constructor TIpHtmlNodeHR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FColor := -1;
  Align := hiaCenter;
  SizeWidth := TIpHtmlPixels.Create;
end;

procedure TIpHtmlNodeHR.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
  SaveBrushColor,
  SavePenColor : TColor;
  aCanvas: TCanvas;
begin
  aCanvas := Owner.Target;
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  if not PageRectToScreen(R, R) then
    Exit;
  if NoShade or (Color <> -1) then begin
    SavePenColor := aCanvas.Pen.Color;
    SaveBrushColor := aCanvas.Brush.Color;
    if Color = -1 then begin
      aCanvas.Pen.Color := clBlack;
      aCanvas.Brush.Color := clBlack;
    end else begin
      aCanvas.Pen.Color := Color;
      aCanvas.Brush.Color := Color;
    end;
    aCanvas.FillRect(R);
    aCanvas.Pen.Color := SavePenColor;
    aCanvas.Brush.Color := SaveBrushColor;
  end else begin
    SavePenColor := aCanvas.Pen.Color;
    SaveBrushColor := aCanvas.Brush.Color;
    aCanvas.Pen.Color := clGray;
    aCanvas.Brush.Color := clGray;
    aCanvas.FillRect(R);
    aCanvas.Pen.Color := clWhite;
    aCanvas.MoveTo(R.Left - 1, R.Bottom + 1);
    aCanvas.LineTo(R.Left - 1, R.Top - 1);
    aCanvas.LineTo(R.Right + 1, R.Top - 1);
    aCanvas.Pen.Color := clBlack;
    aCanvas.LineTo(R.Right + 1, R.Bottom + 1);
    aCanvas.LineTo(R.Left - 1, R.Bottom + 1);
    aCanvas.Pen.Color := SavePenColor;
    aCanvas.Brush.Color := SaveBrushColor;
  end;
end;

function TIpHtmlNodeHR.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    case Width.LengthType of
    hlUndefined :
      FDim.cx := 0;
    hlAbsolute :
      FDim.cx := Width.LengthValue;
    hlPercent :
      FDim.cx := round(ParentWidth * Width.LengthValue / 100);
    end;
    FDim.cy := Size.Value;
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FDim;
end;

function TIpHtmlNodeHR.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeHR.CalcMinMaxWidth(var Min, Max: Integer);
begin
  Min := 0;
  Max := 0;
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := Width.LengthValue;
      Max := Min;
    end;
  end;
end;

procedure TIpHtmlNodeHR.Enqueue;
begin
  EnqueueElement(Owner.SoftLF);
  inherited;
  EnqueueElement(Owner.SoftLF);
end;

destructor TIpHtmlNodeHR.Destroy;
begin
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  FSize.Free;
end;

procedure TIpHtmlNodeHR.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeA }

procedure TIpHtmlNodeA.AddArea(const R: TRect);
var
  RCopy : PRect;
  c : Integer;
begin
  c := AreaList.Count;
  if c > 0 then begin
    RCopy := PRect(AreaList[c-1]);
    if (R.Left = RCopy.Right)
    and (R.Top = RCopy.Top)
    and (R.Bottom = RCopy.Bottom) then begin
      RCopy.Right := R.Right;
      Exit;
    end;
  end;
  New(RCopy);
  RCopy^  := R;
  AreaList.Add(RCopy);
end;

procedure TIpHtmlNodeA.AddMapArea(const R: TRect);
var
  RCopy : PRect;
  c : Integer;
begin
  c := MapAreaList.Count;
  if c > 0 then begin
    RCopy := PRect(AreaList[c-1]);
    if (R.Left = RCopy.Right)
    and (R.Top = RCopy.Top)
    and (R.Bottom = RCopy.Bottom) then begin
      RCopy.Right := R.Right;
      Exit;
    end;
  end;
  New(RCopy);
  RCopy^  := R;
  MapAreaList.Add(RCopy);
end;

procedure TIpHtmlNodeA.ClearAreaList;
var
  a: Pointer;
  m: Pointer;
begin
  while AreaList.Count > 0 do begin
    a:=AreaList[0];
    FreeMem(a);
    AreaList.Delete(0);
  end;
  while MapAreaList.Count > 0 do begin
    m:=MapAreaList[0];
    FreeMem(m);
    MapAreaList.Delete(0);
  end;
end;

constructor TIpHtmlNodeA.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'a';
  {$ENDIF}
  AreaList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
  MapAreaList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
end;

destructor TIpHtmlNodeA.Destroy;
begin
  if HasRef then
    Owner.AnchorList.Remove(Self);
  ClearAreaList;
  AreaList.Free;
  MapAreaList.Free;
  inherited;
end;

procedure TIpHtmlNodeA.BuildAreaList;
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do begin
    TIpHtmlNode(FChildren[i]).ReportDrawRects(AddArea);
    TIpHtmlNode(FChildren[i]).ReportMapRects(AddMapArea);
  end;
end;

function TIpHtmlNodeA.PtInRects(const P: TPoint): Boolean;
var
  i : Integer;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to Pred(AreaList.Count) do begin
    with PRect(AreaList[i])^ do
    if PtInRect(PRect(AreaList[i])^,P) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TIpHtmlNodeA.RelMapPoint(const P: TPoint): TPoint;
var
  i : Integer;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to Pred(MapAreaList.Count) do begin
    with PRect(MapAreaList[i])^ do
    if PtInRect(PRect(AreaList[i])^,P) then begin
      Result := Point(
        P.x - PRect(AreaList[i])^.Left,
        P.y - PRect(AreaList[i])^.Top);
      Exit;
    end;
  end;
  Result := Point(-1, -1);
end;

procedure TIpHtmlNodeA.SetHot(const Value: Boolean);
var
  i : Integer;
  R : TRect;
begin
  FHot := Value;
  if AreaList.Count = 0 then
    BuildAreaList;
  SetProps(Props);
  for i := 0 to Pred(AreaList.Count) do
    if PageRectToScreen(PRect(AreaList[i])^, R) then
      Owner.InvalidateRect(R);
end;

procedure TIpHtmlNodeA.SetHRef(const Value: string);
var
  NewHasRef : Boolean;
begin
  FHRef := Value;
  NewHasRef := Value <> '';
  if NewHasRef <> HasRef then begin
    if HasRef then
      Owner.AnchorList.Remove(Self)
    else
      Owner.AnchorList.Add(Self);
    FHasRef := NewHasRef;
  end;
end;

procedure TIpHtmlNodeA.DoOnBlur;
begin
  {FHasFocus := False;}
  Hot := False;
end;

procedure TIpHtmlNodeA.DoOnFocus;
begin
  {FHasFocus := True;}
  MakeVisible;
  Hot := True;
end;

procedure TIpHtmlNodeA.SetName(const Value: string);
begin
  if FName <> '' then
    with Owner.NameList do
      Delete(IndexOf(FName));
  FName := Value;
  if FName <> '' then
    Owner.NameList.AddObject(FName, Self);
end;

procedure TIpHtmlNodeA.MakeVisible;
var
  i : Integer;
  R : TRect;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  SetRectEmpty(R);
  for i := 0 to Pred(AreaList.Count) do
    UnionRect(R, R, PRect(AreaList[i])^);
  Owner.MakeVisible(R{$IFDEF IP_LAZARUS}, False{$ENDIF});
end;

procedure TIpHtmlNodeA.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  if FHot then begin
    Props.FontColor := Props.LinkColor;
    Props.FontStyle := Props.FontStyle + [fsUnderline];
  end else
    if HasRef then begin
      Props.FontStyle := Props.FontStyle + [fsUnderline];
      if Owner.LinkVisited(HRef) then
        Props.FontColor := Props.VLinkColor
      else
        Props.FontColor := Props.LinkColor;
    end;
  Props.DelayCache:=False;
  inherited SetProps(Props);
end;

function TIpHtmlNodeA.GetHint: string;
begin
  if Title = '' then
    Result := HRef
  else
    Result := Title;
end;

{ TIpHtmlNodeDIV }

constructor TIpHtmlNodeDIV.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'div';
  {$ENDIF}
end;

destructor TIpHtmlNodeDIV.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeDIV.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  {$IFDEF IP_LAZARUS}
  LoadAndApplyCSSProps;
  {$ENDIF}
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeDIV.Enqueue;
begin
  if FChildren.Count > 0 then begin
    if Props.ElemMarginTop.Style=hemsAuto then
      EnqueueElement(Owner.HardLF)
    else begin
      // ToDo: Props.ElemMarginTop
      EnqueueElement(Owner.HardLFClearBoth);
    end;
  end;
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    if Props.ElemMarginTop.Style=hemsAuto then
      EnqueueElement(Owner.HardLF)
    else begin
      // ToDo: Props.ElemMarginTop
      EnqueueElement(Owner.HardLFClearBoth)
    end;
  end;
end;

{ TIpHtmlNodeSPAN }

procedure TIpHtmlNodeSPAN.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.Alignment := Align;
  {$IFDEF IP_LAZARUS}
  LoadAndApplyCSSProps;
  {$ENDIF}
  Props.DelayCache:=False;
end;

constructor TIpHtmlNodeSPAN.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'span';
  {$ENDIF}
end;

{ TIpHtmlNodeTABLE }

procedure TIpHtmlNodeTABLE.CalcMinMaxColTableWidth(
                         const RenderProps: TIpHtmlProps;var Min, Max: Integer);
var
  z, Min0, Max0: Integer;
  i, j, CurCol, k : Integer;
  TWMin, TWMax : Integer;
  PendSpanWidthMin,
  PendSpanWidthMax,
  PendSpanStart,
  PendSpanSpan : TIntArr;
  PendCol : Integer;

  procedure DistributeColSpace(ColSpan: Integer);
  var
    i, Rest, MinNow : Integer;
  begin
    if ColSpan > 1 then begin
      PendSpanWidthMin[PendCol] := Min0;
      PendSpanWidthMax[PendCol] := Max0;
      PendSpanStart[PendCol] := CurCol;
      PendSpanSpan[PendCol] := ColSpan;
      Inc(PendCol);
      Exit;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(MinNow, ColTextWidthMin[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMin[i] := Min0 div ColSpan;
    end else begin
      Rest := Min0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {Inc(ColTextWidthMin[i],
             round(Rest * ColTextWidthMin[i] / MinNow));}
           ColTextWidthMin[i] := ColTextWidthMin[i] +
             round(Rest * ColTextWidthMin[i] / MinNow);
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          Inc(MinNow, ColTextWidthMin[i]);
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            {Inc(ColTextWidthMin[i]);}
            ColTextWidthMin[i] := ColTextWidthMin[i] + 1;
            Dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(MinNow, ColTextWidthMax[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMax[i] := Max0 div ColSpan;
    end else begin
      Rest := Max0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {Inc(ColTextWidthMax[i],
             round(Rest * ColTextWidthMax[i] / MinNow))}
          ColTextWidthMax[i] := ColTextWidthMax[i] +
            round(Rest * ColTextWidthMax[i] / MinNow);
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          Inc(MinNow, ColTextWidthMax[i]);
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            {Inc(ColTextWidthMax[i]);}
            ColTextWidthMax[i] := ColTextWidthMax[i] + 1;
            Dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    for i := 0 to Pred(ColCount) do begin
      ColTextWidthMin[i] := MinI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
    end;
  end;

  procedure DistributeSpannedColSpace;
  var
    z, i, Rest, MinNow, Min0, Max0, CurCol, ColSpan : Integer;
  begin
    for z := 0 to Pred(PendCol) do begin
      Min0 := PendSpanWidthMin[z];
      Max0 := PendSpanWidthMax[z];
      CurCol := PendSpanStart[z];
      ColSpan := PendSpanSpan[z];
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        Inc(MinNow, ColTextWidthMin[i]);
      if MinNow = 0 then begin
        Rest := 0;
        for i := CurCol to CurCol + ColSpan - 1 do begin
           ColTextWidthMin[i] := Min0 div ColSpan;
           Inc(Rest, ColTextWidthMin[i]);
        end;
        ColTextWidthMin[0] := ColTextWidthMin[0] + (Min0 - Rest);
      end else begin
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
             {Inc(ColTextWidthMin[i],
               round(Rest * ColTextWidthMin[i] / MinNow));}
            ColTextWidthMin[i] := ColTextWidthMin[i] +
              round(Rest * ColTextWidthMin[i] / MinNow);
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            Inc(MinNow, ColTextWidthMin[i]);
          Rest := Min0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              {Inc(ColTextWidthMin[i]);}
              ColTextWidthMin[i] := ColTextWidthMin[i] + 1;
              Dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        Inc(MinNow, ColTextWidthMax[i]);
      if MinNow = 0 then begin
        Rest := 0;
        for i := CurCol to CurCol + ColSpan - 1 do begin
           ColTextWidthMax[i] := Max0 div ColSpan;
           Inc(Rest, ColTextWidthMax[i]);
        end;
        ColTextWidthMax[0] := ColTextWidthMax[0] + (Max0 - Rest);
      end else begin
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
             {Inc(ColTextWidthMax[i],
               round(Rest * ColTextWidthMax[i] / MinNow));}
            ColTextWidthMax[i] := ColTextWidthMax[i] +
              round(Rest * ColTextWidthMax[i] / MinNow);
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            Inc(MinNow, ColTextWidthMax[i]);
          Rest := Max0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              {Inc(ColTextWidthMax[i]);}
              ColTextWidthMax[i] := ColTextWidthMax[i] + 1;
              Dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      for i := 0 to Pred(ColCount) do begin
        {ColTextWidthMin[i] := MinI2(ColTextWidthMin[i], ColTextWidthMax[i]);}
        ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      end;
    end;
  end;

begin
  if FMin <> -1 then begin
    Min := FMin;
    Max := FMax;
    Exit;
  end;

  FMin := 0;
  FMax := 0;
  if ColCount = 0 then
    Exit;

  PendSpanWidthMin := nil;
  PendSpanWidthMax := nil;
  PendSpanStart := nil;
  PendSpanSpan := nil;
  try
    PendSpanWidthMin := TIntArr.Create;
    PendSpanWidthMax := TIntArr.Create;
    PendSpanStart := TIntArr.Create;
    PendSpanSpan := TIntArr.Create;

    {calc col and table widths}
    for i := 0 to Pred(ColCount) do begin
      RowSp[i] := 0;
      ColTextWidthMin[i] := 0;
      ColTextWidthMax[i] := 0;
    end;
    PendCol := 0;
    for z := 0 to Pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to Pred(FChildren.Count) do begin
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do begin
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  {Dec(RowSp[CurCol]);}
                  RowSp[CurCol] := RowSp[CurCol] - 1;
                  Inc(CurCol);
                end;
                for j := 0 to Pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      while RowSp[CurCol] <> 0 do begin
                        RowSp[CurCol] := RowSp[CurCol] - 1;
                        Inc(CurCol);
                      end;

                      CalcMinMaxPropWidth(RenderProps, Min0, Max0);

                      case Width.LengthType of
                      hlAbsolute :
                        begin
                          if Width.LengthValue <= ExpParentWidth then
                            Min0 := MaxI2(Min0, Width.LengthValue
                            {$IFDEF IP_LAZARUS}
                              - 2*CellPadding - CellSpacing - RUH);
                            {$ELSE}
                              - 2*CellPadding - 2*CS2 - RUH);
                            {$ENDIF}
                          Max0 := Min0;
                        end;
                      end;

                      FCalcWidthMin := Min0;
                      FCalcWidthMax := Max0;

                      DistributeColSpace(ColSpan);

                      for k := 0 to Pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {Dec(RowSp[CurCol]);}
                          RowSp[CurCol] := RowSp[CurCol] - 1;
                          Inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        Inc(CurCol);
                      end;
                    end;
                for j := CurCol to Pred(ColCount) do
                  if RowSp[j] > 0 then
                    {Dec(RowSp[j]);}
                    RowSp[j] := RowSp[j] - 1;
              end;
          end;

      DistributeSpannedColSpace;
  finally
    PendSpanWidthMin.Free;
    PendSpanWidthMax.Free;
    PendSpanStart.Free;
    PendSpanSpan.Free;
  end;

  TWMin := 0;
  TWMax := 0;
  {$IFDEF IP_LAZARUS}
  CellOverhead := BL + CellSpacing + BR;
  {$ELSE}
  CellOverhead := BL + 2*CS2 + RUH + BR;
  {$ENDIF}
  for i := 0 to Pred(ColCount) do begin
    Inc(TWMin, ColTextWidthMin[i]);
    Inc(TWMax, ColTextWidthMax[i]);
    {$IFDEF IP_LAZARUS}
    Inc(CellOverhead, RUH + 2*CellPadding + CellSpacing + RUH);
    {$ELSE}
    Inc(CellOverhead, 2*CellPadding + 2*CS2 + RUH);
    {$ENDIF}
    RowSp[i] := 0;
  end;

  FMin := MaxI2(FMin, TWMin + CellOverhead);
  FMax := MaxI2(FMax, TWMax + CellOverhead);
  Min := FMin;
  Max := FMax;
end;

procedure TIpHtmlNodeTABLE.SetRect(TargetRect: TRect);
var
  dx,dy : Integer;
  z, i, j : Integer;
  R : TRect;
begin
  if ColCount = 0 then Exit;

  dx := TargetRect.Left - BorderRect2.Left;
  dy := TargetRect.Top - BorderRect2.Top;

  OffsetRect(BorderRect, dx, dy);
  OffsetRect(BorderRect2, dx, dy);
  if FCaption <> nil then begin
    with FCaption do begin
      if not IsRectEmpty(PageRect) then begin
        R := PageRect;
        OffsetRect(R, dx, dy);
        Layout(Props, R);
      end;
    end;
  end;

  for z := 0 to Pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to Pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              for j := 0 to Pred(FChildren.Count) do
                if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                    if not IsRectEmpty(PadRect) then
                      OffsetRect(FPadRect, dx, dy);
                    if not IsRectEmpty(PageRect) then begin
                      R := PageRect;
                      OffsetRect(R, dx, dy);
                      Layout(Props, R);
                    end;
                  end;
            end;
        end;
end;

procedure TIpHtmlNodeTABLE.CalcSize(const ParentWidth: Integer;
  const RenderProps: TIpHtmlProps);
var
  z, GrossCellSpace, NetCellSpace, CellExtra,
  NetCellSpaceExtraExtra,
  RelCellExtra,
  i, j, CurCol, k,
  CellSpace,
  MinW, MaxW : Integer;
  R : TRect;
  TargetRect : TRect;
  RowFixup : TRectRectArr; {PRowSArr;}
  RowFixupCount : Integer;

  function GetSpanBottom(Row, Col: Integer): Integer;
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      Result := R.Bottom
    else
      Result := 0;
  end;

  procedure SetSpanBottom(Row, Col, Value: Integer);
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      R^.Bottom := Value;
  end;

  procedure SetSpanRect(Row,Col : Integer; const Rect: PRect);
  begin
    RowFixup[Row].Value[Col] := Rect;
  end;

  procedure DeleteFirstSpanRow;
  begin
    RowFixup.Delete(0);
  end;

  procedure AdjustCol(ColSpan, DesiredWidth: Integer);
  var
    i, Rest, WNow, Avail : Integer;
  begin
    WNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(WNow, ColTextWidth[i]);
    Avail := MinI2(DesiredWidth, CellSpace);
    if WNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidth[i] := Avail div ColSpan;
    end else begin
      Rest := MinI2(CellSpace, DesiredWidth - WNow);
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {Inc(ColTextWidth[i],
             round(Rest * ColTextWidth[i] / WNow));}
          ColTextWidth[i] := ColTextWidth[i] +
            round(Rest * ColTextWidth[i] / WNow);
      end;
    end;
  end;

  procedure DoBlock(BlockType : TIpHtmlNodeTABLEHEADFOOTBODYClass);
  var
    z, i, j, k, zz : Integer;
    RowSp2 : TIntArr;
    AL0, AL : TIpHtmlAlign;
    CellRect1 : TRect;
    HA, HB, Y0: Integer;
    maxY, maxYY: Integer;
    VA0, VA : TIpHtmlVAlign3;
  begin
    RowSp2 := TIntArr.Create;
    try
      for z := 0 to Pred(FChildren.Count) do
        if (TIpHtmlNode(FChildren[z]) is BlockType) then
          with TIpHtmlNodeCore(FChildren[z]) do
            for i := 0 to Pred(FChildren.Count) do begin
              if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
                with TIpHtmlNodeTR(FChildren[i]) do begin

                  for j := 0 to Pred(ColCount) do
                    RowSp2[j] := RowSp[j];

                  CurCol := 0;
                  while RowSp[CurCol] <> 0 do begin
                    RowSp[CurCol] := RowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;

                  VA0 := Props.VAlignment;
                  case VAlign of
                  hvaTop :
                    VA0 := hva3Top;
                  hvaMiddle :
                    VA0 := hva3Middle;
                  hvaBottom :
                    VA0 := hva3Bottom;
                  end;

                  case Align of
                  haDefault :
                    AL0 := haLeft;
                  else
                    AL0 := Align;
                  end;

                  {determine height of cells and lay out with top alignment}
                  for j := 0 to Pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin
                        while RowSp[CurCol] <> 0 do begin
                          RowSp[CurCol] := RowSp[CurCol] - 1;
                          Inc(CurCol);
                        end;

                        AL := AL0;

                        Props.Assign(Self.Props); // assign table props

                        CellRect1 := TargetRect;

                        Inc(CellRect1.Left,
                          ColStart[CurCol]);

                        {$IFDEF IP_LAZARUS}
                        Inc(CellRect1.Top, CellSpacing + RUV);
                        {$ELSE}
                        Inc(CellRect1.Top, CS2 + RUV);
                        {$ENDIF}

                        CellRect1.Right :=
                          CellRect1.Left
                          + 2*CellPadding
                          + ColTextWidth[CurCol]
                          {$IFDEF IP_LAZARUS}
                          ;
                          {$ELSE}
                          + 2*CS2;
                          {$ENDIF}

                        for k := 1 to ColSpan - 1 do
                          Inc(CellRect1.Right,
                            ColTextWidth[CurCol + k] +
                            2*CellPadding +
                            {$IFDEF IP_LAZARUS}
                            2*RUH +
                            CellSpacing);
                            {$ELSE}
                            2*CS2 + RUH);
                            {$ENDIF}
                            
                        {$IFDEF IP_LAZARUS}
                        // PadRect area of cell excluding rules
                        // CellRect area of text contained in cell
                        FPadRect := CellRect1;
                        Inc(CellRect1.Top, CellPadding);
                        inflateRect(CellRect1, -CellPadding, 0);
                        {$ELSE}
                        FPadRect := CellRect1;
                        InflateRect(FPadRect, -CS2, 0);

                        Inc(CellRect1.Top, CellPadding);
                        InflateRect(CellRect1, -(CellPadding + CS2), 0);
                        {$ENDIF}

                        VA := VAlign;
                        if VA = hva3Default then
                          VA := VA0;

                        case Align of
                        haDefault : ;
                        else
                          AL := Align;
                        end;

                        Props.VAlignment := VA;
                        Props.Alignment := AL;
                        Layout(Props, CellRect1);

                        if (Height.PixelsType <> hpUndefined) {Height <> -1} then
                          if PageRect.Bottom - PageRect.Top < Height.Value then
                            FPageRect.Bottom := CellRect1.Top + Height.Value;

                        if (Height.PixelsType = hpUndefined) {Height = -1}
                        and IsRectEmpty(PageRect) then
                          FPadRect.Bottom := CellRect1.Top + CellPadding
                        else begin
                          FPadRect.Bottom := PageRect.Bottom + CellPadding;
                        end;
                        SetSpanRect(RowSpan - 1, CurCol, @PadRect);

                        for k := 0 to Pred(ColSpan) do begin
                          RowSp[CurCol] := RowSpan - 1;
                          Inc(CurCol);
                        end;
                      end;

                  {Adjust any trailing spanning columns}
                  for j := CurCol to Pred(ColCount) do
                    if RowSp[j] > 0 then
                      {Dec(RowSp[j]);}
                      RowSp[j] := RowSp[j] - 1;

                  maxYY := 0;
                  maxY := 0;
                  {if RowFixupCount > 0 then begin}
                    for zz := 0 to Pred(ColCount) do
                      maxY := MaxI2(GetSpanBottom(0, zz), maxY);
                    for zz := 0 to Pred(ColCount) do
                      SetSpanBottom(0, zz, maxY);
                   if maxY > maxYY then
                     maxYY := maxY;
                  {end;}

                  for j := 0 to Pred(ColCount) do
                    RowSp[j] := RowSp2[j];

                  CurCol := 0;
                  while RowSp[CurCol] <> 0 do begin
                    {Dec(RowSp[CurCol]);}
                    RowSp[CurCol] := RowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;
                  {relocate cells which are not top aligned}
                  for j := 0 to Pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {Dec(RowSp[CurCol]);}
                          RowSp[CurCol] := RowSp[CurCol] - 1;
                          Inc(CurCol);
                        end;

                        AL := AL0;

                        {$IFDEF IP_LAZARUS}
                        HA := maxYY - (TargetRect.Top + CellSpacing + RUV);
                        {$ELSE}
                        HA := maxYY - TargetRect.Top;
                        {$ENDIF}
                        HB := PageRect.Bottom - PageRect.Top;

                        VA := VAlign;
                        if VA = hva3Default then
                          VA := VA0;

                        case VA of
                        hva3Middle :
                          Y0 := (HA - HB) div 2;
                        hva3Bottom :
                          Y0 := (HA - HB);
                        else
                          Y0 := 0;
                        end;

                        if Y0 > 0 then begin

                          CellRect1 := TargetRect;

                          Inc(CellRect1.Left,
                            ColStart[CurCol]);

                          {$IFDEF IP_LAZARUS}
                          Inc(CellRect1.Top, CellSpacing + RUV + Y0);
                          {$ELSE}
                          Inc(CellRect1.Top, CS2 + RUV + Y0);

                          {$ENDIF}
                          CellRect1.Right :=
                            CellRect1.Left
                            + 2*CellPadding
                            + ColTextWidth[CurCol]
                            {$IFDEF IP_LAZARUS}
                            ;
                            {$ELSE}
                            + 2*CS2;
                            {$ENDIF}

                          for k := 1 to ColSpan - 1 do
                            Inc(CellRect1.Right,
                              ColTextWidth[CurCol + k] +
                              2*CellPadding +
                              {$IFDEF IP_LAZARUS}
                              2*RUH + CellSpacing);
                              {$ELSE}
                              2*CS2 + RUH);
                              {$ENDIF}

                          Inc(CellRect1.Top, CellPadding);
                          {$IFDEF IP_LAZARUS}
                          inflateRect(CellRect1, -CellPadding, 0);
                          {$ELSE}
                          InflateRect(CellRect1, -(CellPadding + CS2), 0);
                          {$ENDIF}

                          case Align of
                          haDefault : ;
                          else
                            AL := Align;
                          end;

                          Props.VAlignment := VA;
                          Props.Alignment := AL;

                          Layout(Props, CellRect1);

                          if Height.PixelsType <> hpUndefined {Height <> -1} then
                            if PageRect.Bottom - PageRect.Top < Height.Value then
                              FPageRect.Bottom := CellRect1.Top + Height.Value;

                          if (Height.PixelsType = hpUndefined) {(Height = -1)}
                          and IsRectEmpty(PageRect) then
                            FPadRect.Bottom := CellRect1.Top + CellPadding
                          else begin
                            FPadRect.Bottom := PageRect.Bottom + CellPadding;
                          end;
                          SetSpanRect(RowSpan - 1, CurCol, @PadRect);

                        end;

                        for k := 0 to Pred(ColSpan) do begin
                          RowSp[CurCol] := RowSpan - 1;
                          Inc(CurCol);
                        end;
                      end;

                  maxYY := 0;
                  maxY := 0;

                  {if RowFixupCount > 0 then begin}
                    for zz := 0 to Pred(ColCount) do
                      maxY := MaxI2(GetSpanBottom(0, zz), maxY);
                    for zz := 0 to Pred(ColCount) do
                      SetSpanBottom(0, zz, maxY);
                    if maxY > maxYY then
                      maxYY := maxY;
                  {end;}

                  {Adjust any trailing spanning columns}
                  for j := CurCol to Pred(ColCount) do
                    if RowSp[j] > 0 then
                      {Dec(RowSp[j]);}
                      RowSp[j] := RowSp[j] - 1;

                  {$IFDEF IP_LAZARUS}
                  TargetRect.Top := MaxI2(maxYY, TargetRect.Top) + RUV;
                  {$ELSE}
                  TargetRect.Top := MaxI2(maxYY, TargetRect.Top);

                  {$ENDIF}
                  DeleteFirstSpanRow;
                end;
            end;

      while RowFixupCount > 0  do begin
        maxYY := 0;
        maxY := 0;
        for zz := 0 to Pred(ColCount) do
          maxY := MaxI2(GetSpanBottom(0, zz), maxY);
        for zz := 0 to Pred(ColCount) do
          SetSpanBottom(0, zz, maxY);
        if maxY > maxYY then
          maxYY := maxY;

        TargetRect.Top := MaxI2(maxYY, TargetRect.Top);

        DeleteFirstSpanRow;
      end;

    finally
      RowSp2.Free;
    end;
  end;

var
  P : Integer;
begin
  FTableWidth := 0;
  if ColCount = 0 then
    Exit;
  Props.Assign(RenderProps);
  CalcMinMaxColTableWidth(Props, MinW, MaxW);

  case Width.LengthType of
  hlUndefined :
    begin
      P := 0;
      for z := 0 to Pred(FChildren.Count) do
        if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
          with TIpHtmlNodeCore(FChildren[z]) do
            for i := 0 to Pred(FChildren.Count) do begin
              if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
                with TIpHtmlNodeTR(FChildren[i]) do begin
                  for j := 0 to Pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                        case Width.LengthType of
                        hlPercent :
                          Inc(P, Width.LengthValue);
                        end;
                      end;
                end;
            end;
      if P <> 0 then
        FTableWidth := MaxI2(MinW, round((P * ParentWidth) / 100))
      else
        FTableWidth := MaxI2(MinW, MinI2(MaxW, ParentWidth));
    end;
  hlAbsolute :
    FTableWidth := MaxI2(Width.LengthValue, MinW);
  hlPercent :
    FTableWidth := MaxI2(MinW, round((Width.LengthValue * ParentWidth) / 100));
  end;

  for i := 0 to Pred(ColCount) do
    ColTextWidth[i] := ColTextWidthMin[i];

  for z := 0 to Pred(ColCount) do
    RowSp[z] := 0;

  for z := 0 to Pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to Pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              CellSpace := FTableWidth - CellOverhead;
              for j := 0 to Pred(ColCount) do
                Dec(CellSpace, ColTextWidth[j]);

              if CellSpace > 0 then begin
                {distribute extra space}
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  RowSp[CurCol] := RowSp[CurCol] - 1;
                  Inc(CurCol);
                end;
                for j := 0 to Pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlAbsolute :
                        AdjustCol(ColSpan, Width.LengthValue -
                        {$IFDEF IP_LAZARUS}
                          2*CellPadding - CellSpacing - RUH);
                        {$ELSE}
                          2*CellPadding - 2*CS2 - RUH);
                        {$ENDIF}
                      hlPercent :
                        AdjustCol(Colspan,
                                  round((FTableWidth - CellOverhead) *
                                        Width.LengthValue / 100));
                      end;

                      CellSpace := FTableWidth - CellOverhead;
                      for k := 0 to Pred(ColCount) do
                        Dec(CellSpace, ColTextWidth[k]);

                      for k := 0 to Pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          RowSp[CurCol] := RowSp[CurCol] - 1;
                          Inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        Inc(CurCol);
                      end;
                    end;
                for j := CurCol to Pred(ColCount) do
                  if RowSp[j] > 0 then
                    RowSp[j] := RowSp[j] - 1;
              end;
            end;
        end;

  GrossCellSpace := MaxI2(FTableWidth - CellOverhead, 0);
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  if NetCellSpace > 0 then begin
    CellExtra := GrossCellSpace - NetCellSpace;
    if CellExtra > 0 then
      for i := 0 to Pred(ColCount) do begin
        RelCellExtra := round(CellExtra / NetCellSpace * ColTextWidth[i] );
        if ColTextWidth[i] + RelCellExtra > ColTextWidthMax[i] then
          ColTextWidth[i] := MaxI2(ColTextWidth[i], ColTextWidthMax[i])
        else
          ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      end;
  end;

  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to Pred(ColCount) do begin
      if (ColTextWidth[i] < ColTextWidthMax[i]) then begin
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
        if NetCellSpaceExtraExtra > 0 then begin
          ColTextWidth[i] := ColTextWidth[i] + 1;
          Dec(NetCellSpaceExtraExtra);
        end;
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    for i := 0 to Pred(ColCount) do begin
      RelCellExtra := MinI2(ColTextWidthMax[i] - ColTextWidth[i], CellExtra);
      if RelCellExtra > 0 then begin
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
        Dec(CellExtra, RelCellExtra);
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to Pred(ColCount) do begin
      ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      if NetCellSpaceExtraExtra > 0 then begin
        ColTextWidth[i] := ColTextWidth[i] + 1;
        Dec(NetCellSpaceExtraExtra);
      end;
    end;
  end;

  for i := 0 to Pred(ColCount) do
    RowSp[i] := 0;

  TargetRect := Rect(0, 0, ParentWidth, MaxInt);

  BorderRect2 := TargetRect;
  BorderRect := TargetRect;

  for z := 0 to Pred(FChildren.Count) do
    if TIpHtmlNode(FChildren[z]) is TIpHtmlNodeCAPTION then begin
      FCaption := TIpHtmlNodeCAPTION(FChildren[z]);
      if FCaption.Align <> hva2Bottom then begin
        FCaption.Layout(Props, BorderRect2);
        Inc(BorderRect.Top, FCaption.PageRect.Bottom - FCaption.PageRect.Top);
      end;
    end;

  TargetRect := BorderRect;

  R := BorderRect;

  {$IFDEF IP_LAZARUS}
  ColStart[0] := BL + CellSpacing + RUH;
  {$ELSE}
  ColStart[0] := BL + CS2 + RUH;
  {$ENDIF}
  RowSp[0] := 0;
  for i := 1 to Pred(ColCount) do begin
    ColStart[i] :=
      ColStart[i-1]
      + 2*CellPadding
      + ColTextWidth[i-1]
      {$IFDEF IP_LAZARUS}
      + CellSpacing
      + 2*RUH;
      {$ELSE}
      + 2*CS2
      + RUH;
      {$ENDIF}
    RowSp[i] := 0;
  end;

  {calc size of table body}

  Inc(TargetRect.Top, BT);

  {calc rows}

  RowFixup := TRectRectArr.Create;
  try
    RowFixupCount := 0;

    DoBlock(TIpHtmlNodeTHEAD);
    DoBlock(TIpHtmlNodeTBODY);
    DoBlock(TIpHtmlNodeTFOOT);
  finally
    RowFixup.Free;
  end;
  
  {$IFDEF IP_LAZARUS}
  Inc(TargetRect.Top, CellSpacing + RUV + BB);
  {$ELSE}
  Inc(TargetRect.Top, CS2 + RUV + BB);
  {$ENDIF}

  R.Right := R.Left + FTableWidth;
  R.Bottom := TargetRect.Top;

  if (R.Bottom > R.Top) and (R.Right = R.Left) then
    R.Right := R.Left + 1;

  BorderRect.BottomRight := R.BottomRight;
  BorderRect2.BottomRight := R.BottomRight;

  if assigned(FCaption) and (FCaption.Align = hva2Bottom) then begin
    R.Top := BorderRect.Bottom;
    R.Bottom := MaxInt;
    FCaption.Layout(Props, R);
    BorderRect2.Bottom := FCaption.PageRect.Bottom;
  end;
end;

constructor TIpHtmlNodeTABLE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'table';
  {$ENDIF}
  BgColor := -1;
  SizeWidth := TIpHtmlPixels.Create;
  SizeWidth.PixelsType := hpUndefined;
  FColCount := -1;
  FMin := -1;
  FMax := -1;
  FBorderColor := $808080;
  FBorderStyle := cbsInset;
  ColTextWidth := TIntArr.Create;
  ColStart := TIntArr.Create;
  ColTextWidthMin := TIntArr.Create;
  ColTextWidthMax := TIntArr.Create;
  RowSp := TIntArr.Create;
end;

procedure TIpHtmlNodeTABLE.Draw(Block: TIpHtmlNodeBlock);
var
  z, i, j : Integer;
  R : TRect;
  Al : TIpHtmlVAlign3;
  TRBgColor, TrTextColor: TColor;
  aCanvas : TCanvas;
begin
  aCanvas := Owner.Target;
  if (Props.BGColor <> -1) and PageRectToScreen(BorderRect, R) then begin
    aCanvas.Brush.Color := Props.BGColor;
    aCanvas.FillRect(R);
  end;
  aCanvas.Pen.Color := clBlack;

  Al := Props.VAlignment;

  for z := 0 to Pred(ColCount) do
    RowSp[z] := 0;

  for z := 0 to Pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to Pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              case VAlign of
              hvaTop :
                Al := hva3Top;
              hvaMiddle :
                Al := hva3Middle;
              hvaBottom :
                Al := hva3Bottom;
              end;

              TrBgColor := BgColor;
              TrTextColor := TextColor;

              for j := 0 to Pred(FChildren.Count) do
                if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                    case VAlign of
                    hva3Default :
                      ;
                    else
                      Al := VAlign;
                    end;

                    // set TR color, Render override them anyway if TD/TH have own settings
                    if TrBgColor <> -1 then
                      Props.BGColor := TrBgColor;

                    if TrTextColor <> -1 then
                      Props.FontColor := TrTextColor;

                    Props.VAlignment := Al;
                    Render(Props);
                    {paint left rule if selected}
                    case Rules of
                    hrNone,
                    hrGroups :;
                    hrRows :;
                    hrCols,
                    hrAll :
                      begin
                        if not IsRectEmpty(PadRect) then begin
                          R := PadRect;
                          Inflaterect(R, 1, 1);
                          {$IFDEF IP_LAZARUS}
                          ScreenFrame(R, False);
                          {$ELSE}
                          ScreenRect(R, RGB(192,192,192));
                          {$ENDIF}
                        end;
                      end;
                    end;

                  end;

            end;
        end;

  {render frames}
  // to frame
  if Frame in [hfAbove, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Right-1, BorderRect.Top),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfAbove))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Right, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1), BorderRect.Top + Border - 1),
        Point(BorderRect.Left + Border - 1, BorderRect.Top + Border - 1)],
        CalcBorderColor(BorderColor, BorderStyle, hfAbove));
  // bottom frame
  if Frame in [hfBelow, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfBelow))
    else
    ScreenPolygon(
      [
      Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
      Point(BorderRect.Right - (Border - 1), BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left + Border, BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left, BorderRect.Bottom - 1)],
        CalcBorderColor(BorderColor, BorderStyle, hfBelow));
  // left frame
  if Frame in [hfLhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfLhs))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        Point(BorderRect.Left + (Border - 1), BorderRect.Bottom - Border),
        Point(BorderRect.Left + (Border - 1), BorderRect.Top + (Border - 1))],
        CalcBorderColor(BorderColor, BorderStyle, hfLhs));
  // right frame
  if Frame in [hfRhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfRhs))
    else
      ScreenPolygon(
        [
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Top + (Border - 1)),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Bottom - Border)],
        CalcBorderColor(BorderColor, BorderStyle, hfRhs));

  {render caption}
  if assigned(FCaption) then
    FCaption.Render(Props);
end;

procedure TIpHtmlNodeTABLE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := False;
  inherited SetProps(RenderProps);
end;

function TIpHtmlNodeTABLE.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute)
  or (SizeWidth.Value <> ParentWidth) then begin
    SizeWidth.PixelsType := hpUndefined;
    CalcSize(ParentWidth, Props);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result :=
    SizeRec(BorderRect2.Right - BorderRect2.Left,
      BorderRect2.Bottom - BorderRect2.Top);
end;

procedure TIpHtmlNodeTABLE.CalcMinMaxWidth(var Min, Max: Integer);
begin
  CalcMinMaxColTableWidth(Props, Min, Max);
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := MaxI2(Min, Width.LengthValue);
      Max := MaxI2(Max, Min);
    end;
  end;
end;

procedure TIpHtmlNodeTABLE.InvalidateSize;
begin
  SizeWidth.PixelsType := hpUndefined;
  FMin := -1;
  FMax := -1;
  inherited;
end;

function TIpHtmlNodeTABLE.GetColCount: Integer;
var
  z, i, j, c : Integer;
begin
  if FColCount = -1 then begin
    FColCount := 0;
    for z := 0 to Pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to Pred(FChildren.Count) do begin
            c := 0;
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do
                for j := 0 to Pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do
                      Inc(c, Colspan);
            if c > FColCount then
              FColCount := c;
          end;
    {$IFNDEF IP_LAZARUS}
    CS2 := CellSpacing div 2;
    if (CellSpacing > 0) and (CS2 = 0) then
      CS2 := 1;
    {$ENDIF}
    RUH := 0;
    RUV := 0;
    case Rules of
    hrNone :;
    hrGroups :
      begin
        RUH := 1;
        RUV := 1;
      end;
    hrRows :
      RUV := 1;
    hrCols :
      RUH := 1;
    hrAll :
      begin
        RUH := 1;
        RUV := 1;
      end;
    end;
    BL := 0; BR := 0;
    BT := 0; BB := 0;
    case Frame of
    hfVoid,
    hfAbove :
      BT := Border;
    hfBelow :
      BB := Border;
    hfHSides :
      begin
        BT := Border;
        BB := Border;
      end;
    hfLhs :
      BL := Border;
    hfRhs :
      BR := Border;
    hfvSides :
      begin
        BL := Border;
        BR := Border;
      end;
    hfBox,
    hfBorder :
      begin
        BT := Border;
        BB := Border;
        BL := Border;
        BR := Border;
      end;
    end;
  end;
  Result := FColCount;
end;

procedure TIpHtmlNodeTABLE.Enqueue;
begin
//The commented code bellow prevent a blank line before the table
{
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
}
  EnqueueElement(Element);
{
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
}
end;

procedure TIpHtmlNodeTABLE.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  if Border = 0 then begin
    Frame := hfVoid;
    Rules := hrNone;
  end else begin
    Frame := hfBorder;
    Rules := hrAll;
  end;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetCellPadding(const Value: Integer);
begin
  FCellPadding := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetCellSpacing(const Value: Integer);
begin
  FCellSpacing := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetFrame(const Value: TIpHtmlFrameProp);
begin
  FFrame := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetRules(const Value: TIpHtmlRules);
begin
  FRules := Value;
  InvalidateSize;
end;

destructor TIpHtmlNodeTABLE.Destroy;
begin
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  ColTextWidth.Free;
  ColStart.Free;
  ColTextWidthMin.Free;
  ColTextWidthMax.Free;
  RowSp.Free;
end;

procedure TIpHtmlNodeTABLE.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

function TIpHtmlNodeTABLE.ExpParentWidth: Integer;
begin
  case Width.LengthType of
  hlAbsolute :
    Result := Width.LengthValue;
  else
    Result := inherited ExpParentWidth;
  end;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlNodeTABLE.LoadAndApplyCSSProps;
begin
  inherited LoadAndApplyCSSProps;
  if FCombinedCSSProps = nil then
    exit;
//  if FCombinedCSSProps.BGColor <> -1 then
//    BgColor := FCombinedCSSProps.BGColor;
  if FCombinedCSSProps.Border.Style <> cbsNone then
  begin
    FBorder := FCombinedCSSProps.Border.Width;
    BorderColor := FCombinedCSSProps.Border.Color;
    BorderStyle := FCombinedCSSProps.Border.Style;
    if Frame = hfVoid then
    begin
      Frame := hfBorder;
      Rules := hrGroups;
    end;
  end;
end;
{$ENDIF}

procedure TIpHtmlNodeTR.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontColor := TextColor;
  Props.BgColor := BgColor;
  inherited SetProps(Props);
end;

constructor TIpHtmlNodeTR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'tr';
  {$ENDIF}
  FAlign := haDefault;
  FValign := hvaMiddle;
end;

{ TIpHtmlNodeMAP }

constructor TIpHtmlNodeMAP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Owner.MapList.Add(Self);
end;

destructor TIpHtmlNodeMAP.Destroy;
begin
  Owner.MapList.Remove(Self);
  inherited;
end;

{ TIpHtmlNodeAREA }

destructor TIpHtmlNodeAREA.Destroy;
var
  I: Integer;
begin
  I := Owner.AreaList.IndexOf(Self);
  if I <> -1 then
    Owner.AreaList.Delete(I);
  inherited;
end;

function TIpHtmlNodeAREA.GetHint: string;
begin
  if Alt <> '' then
    Result := Alt
  else
    Result := HRef;
end;

function TIpHtmlNodeAREA.PtInRects(const P: TPoint): Boolean;
begin
  if PtInRect(FRect, P) then
    Result := True
  else
  if FRgn <> 0 then
    Result := PtInRegion(FRgn, P.x, P.y)
  else
    Result := False;
end;

procedure TIpHtmlNodeAREA.Reset;
begin
  if FRgn <> 0 then
    DeleteObject(FRgn);
  SetRectEmpty(FRect);
end;

{ TIpHtmlNodeIMG }

procedure TIpHtmlNodeIMG.LoadImage;
begin
  if Src <> '' then begin
    if FPicture <> Owner.DefaultImage then begin
      FPicture.Free;
      FPicture := nil;
    end;
    Owner.DoGetImage(Self, Owner.BuildPath(Src), FPicture);
    if FPicture = nil
      then FPicture := Owner.DefaultImage;

   {$IFDEF IP_LAZARUS}
      {$IFDEF UseGifImageUnit}
      if (FPicture <> nil)
      and (FPicture.Graphic <> nil)
      and (FPicture.Graphic is TGifImage)
      then
        Owner.GifImages.Add(Self);
      {$ELSE}
      if (FPicture <> nil)
      and (FPicture.Graphic <> nil)
      and (FPicture.Graphic is TIpAnimatedGraphic)
      then
        Owner.AnimationFrames.Add(Self);
      {$ENDIF}
    {$ELSE}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil) then  begin
      if  FPicture.Graphic is TGifImage
      then  Owner.GifImages.Add(Self)
      else  Owner.OtherImages.Add(Self);
    end;
    {$ENDIF}
  end;
end;

procedure TIpHtmlNodeIMG.UnloadImage;
begin
  {$IFDEF IP_LAZARUS}
    {$IFDEF UseGifImageUnit}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TGifImage)
    then
      Owner.GifImages.Remove(Self);
    {$ELSE}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TIpAnimatedGraphic)
    then
      Owner.AnimationFrames.Remove(Self);
    {$ENDIF}
  {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)  then  begin
    if  FPicture.Graphic is TGifImage
    then  Owner.GifImages.Remove(Self)
    else  Owner.OtherImages.Remove(Self);
  end;
  {$ENDIF}
  if FPicture <> Owner.DefaultImage then begin
    FPicture.Free;
    FPicture := nil;
  end;
end;

destructor TIpHtmlNodeIMG.Destroy;
begin
  UnloadImage;
  UseMap := '';
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  FHeight.Free;
end;

{$IFDEF IP_LAZARUS}
function TIpHtmlNodeIMG.GetBorder: Integer;
begin
  if (FPicture<>nil)and(FPicture.Graphic=nil) then
    Result := 1
  else
    Result := fBorder;
end;
{$ENDIF}
procedure TIpHtmlNodeIMG.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
begin
  if FPicture = nil then
    LoadImage;

  if (FPicture <> nil) and (FPicture.Graphic = nil) then
    LoadImage;
  Owner.AddRect(GrossDrawRect, Element, Block);
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  
  if Border <> 0 then begin
    if Border = 1 then begin
      ScreenLine(
        R.TopLeft,
        Point(R.Right, R.Top),
        1,
        RGB(220,220,220));
      ScreenLine(
        R.BottomRight,
        Point(R.Left, R.Bottom),
        1,
        RGB(64,64,64));
      ScreenLine(
        R.TopLeft,
        Point(R.Left, R.Bottom),
        1,
        RGB(192,192,192));
      ScreenLine(
        R.BottomRight,
        Point(R.Right, R.Top),
        1,
        RGB(128,128,128));
    end else begin
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + Border - 1),
        Point(R.Left + Border - 1, R.Top + Border - 1)],
        RGB(220,220,220));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - Border, R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left, R.Bottom - 1)],
          RGB(64,64,64));
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Left, R.Bottom - 1),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Top + (Border - 1))],
        RGB(192,192,192));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + (Border - 1)),
        Point(R.Right - Border, R.Bottom - Border)],
        RGB(128,128,128));
    end;
    InflateRect(R, -Border, -Border);
  end;

  InflateRect(R, -HSpace, -VSpace);

  if FPicture <> nil then begin
  {$IFDEF IP_LAZARUS}
    if FPicture.Graphic=nil then begin
      if PageRectToScreen(R,R) then
        Owner.Target.TextRect(R, R.Left, R.Top, GetHint);
      Exit;
    end;
  {$ENDIF}
    FPicture.Graphic.Transparent := True;
    NetDrawRect := R;
    if PageRectToScreen(R, R) then begin
      {$IFDEF IP_LAZARUS}
        {$IFDEF UseGifImageUnit}
        if (FPicture.Graphic is TGifImage)
        and (TGifImage(FPicture.Graphic).Images.Count > 1) then begin
          TGifImage(FPicture.Graphic).DrawOptions :=
            TGifImage(FPicture.Graphic).DrawOptions + [goDirectDraw];
          Owner.AddGifQueue(FPicture.Graphic, R);
        end else
        {$ELSE}
        if (FPicture.Graphic is TIpAnimatedGraphic)
        and (TIpAnimatedGraphic(FPicture.Graphic).Images.Count > 1) then begin
          TIpAnimatedGraphic(FPicture.Graphic).AggressiveDrawing := True;
          Owner.AddGifQueue(FPicture.Graphic, R);
        end else
      begin
        {$ENDIF}
      {$ENDIF}
        if FPicture = Owner.DefaultImage then begin
          if ((NetDrawRect.Right - NetDrawRect.Left) > FPicture.Graphic.Width)
          and ((NetDrawRect.Bottom - NetDrawRect.Top) > FPicture.Graphic.Height) then begin
            Owner.Target.Brush.Color := Props.FontColor;
            Owner.Target.FrameRect(R);
            Owner.Target.Draw(R.Left + 1, R.Top + 1, FPicture.Graphic);
          end else
            Owner.Target.StretchDraw(R, FPicture.Graphic);
        end else
          Owner.Target.StretchDraw(R, FPicture.Graphic);
      {$IFDEF IP_LAZARUS}
      end;
      {$ENDIF}
    end;
  end
end;

function TIpHtmlNodeIMG.GrossDrawRect : TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeIMG.ReportDrawRects(M: TRectMethod);
begin
  M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ReportMapRects(M: TRectMethod);
begin
  if IsMap then
    M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ImageChange(NewPicture: TPicture);
var
  OldDim,
  Dim : TSize;
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  OldDim := GetDim(-1);
  {$IFDEF IP_LAZARUS}
    {$IFDEF UseGifImageUnit}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TGifImage)
    then
      Owner.GifImages.Remove(Self);
    {$ELSE}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TIpAnimatedGraphic)
    then
      Owner.AnimationFrames.Remove(Self);
    {$ENDIF}
 {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil) then  begin
    if  FPicture.Graphic is TGifImage
    then  Owner.GifImages.Remove(Self)
    else  Owner.OtherImages.Remove(Self);
  end;
 {$ENDIF}
  if FPicture <> Owner.DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  {$IFDEF IP_LAZARUS}
    {$IFDEF UseGifImageUnit}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TGifImage)
    then
      Owner.GifImages.Add(Self);
    {$ELSE}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TIpAnimatedGraphic)
    then
      Owner.AnimationFrames.Add(Self);
    {$ENDIF}
 {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil) then  begin
    if  FPicture.Graphic is TGifImage
    then  Owner.GifImages.Add(Self)
    else  Owner.OtherImages.Add(Self);
  end;
 {$ENDIF}
  SizeWidth.PixelsType := hpUndefined;
  Dim := GetDim(0);
  if (Dim.cx <> OldDim.cx)
  or (Dim.cy <> OldDim.cy) then
    InvalidateSize
  else
    Invalidate;
end;

procedure TIpHtmlNodeIMG.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
end;

function TIpHtmlNodeIMG.GetDim(ParentWidth: Integer): TSize;
var
  DimKnown, NoLoad : Boolean;
begin
  if ParentWidth < 0 then begin
    NoLoad := True;
    ParentWidth := 0;
  end else
    NoLoad := False;
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    DimKnown := True;
    if (Height.PixelsType <> hpUndefined)
    and (Width.LengthType <> hlUndefined) then begin
      case Width.LengthType of
      hlUndefined :
        DimKnown := False;
      hlAbsolute :
        begin
          FSize := SizeRec(Width.LengthValue, Height.Value);
        end;
      hlPercent :
        begin
          FSize := SizeRec(
            round(ParentWidth * Width.LengthValue / 100)
               - 2*HSpace - 2*Border,
            Height.Value);
        end;
      end;
    end else
      DimKnown := False;
    if not DimKnown then begin
      if (FPicture <> nil) then begin
        {$IFDEF IP_LAZARUS}
        if FPicture.Graphic=nil then
          // todo: needs to return the "text size" of GetHint
          FSize := SizeRec(100,20)
        else
        {$ENDIF}
        if ScaleBitmaps then
          FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
        else
          FSize := SizeRec(FPicture.Width, FPicture.Height)
      end else begin
        if NoLoad then
          FSize := SizeRec(0, 0)
        else begin
          LoadImage;
          if FPicture <> nil then begin
            if ScaleBitmaps then
              FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
            else
              {$IFDEF IP_LAZARUS}
              if FPicture.Graphic=nil then
                // todo: needs to return the "text size" of GetHint
                FSize := SizeRec(100,20)
              else
              {$ENDIF}
              FSize := SizeRec(FPicture.Width, FPicture.Height);
          end else
            FSize := SizeRec(0, 0);
        end;
      end;
      if FPicture <> nil then begin
        case Width.LengthType of
        hlUndefined :;
        hlAbsolute :
          begin
            FSize := SizeRec(Width.LengthValue, FSize.cy);
          end;
        hlPercent :
          begin
            FSize := SizeRec(
              round(ParentWidth * Width.LengthValue / 100)
               - 2*HSpace - 2*Border,
              FSize.cy);
          end;
        end;
        if Height.PixelsType <> hpUndefined {Height <> -1} then
          FSize.cy := Height.Value;
      end;
    end;
    FSize := SizeRec(FSize.cx + 2*HSpace + 2*Border, FSize.cy + 2*VSpace + 2*Border);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FSize;
end;

procedure TIpHtmlNodeIMG.CalcMinMaxWidth(var Min, Max: Integer);
var
  Dim : TSize;
begin
  Dim := GetDim(0);
  Min := Dim.cx;
  Max := Min;
end;

procedure TIpHtmlNodeIMG.SetUseMap(const Value: string);
begin
  if FUseMap <> '' then begin
    Owner.MapImgList.Remove(Self);
    Owner.ClearAreaList;
  end;
  FUseMap := Value;
  if FUseMap <> '' then begin
    Owner.MapImgList.Add(Self);
    Owner.ClearAreaList;
  end;
end;

function TIpHtmlNodeIMG.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeIMG.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetHSpace(const Value: Integer);
begin
  FHSpace := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetVSpace(const Value: Integer);
begin
  FVSpace := Value;
  InvalidateSize;
end;

constructor TIpHtmlNodeIMG.Create;
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'img';
  {$ENDIF}
  SizeWidth := TIpHtmlPixels.Create;
end;

procedure TIpHtmlNodeIMG.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.InvalidateSize;
begin
  inherited;
  SizeWidth.PixelsType := hpUndefined;
end;

{ TIpHtmlNodeFORM }

constructor TIpHtmlNodeFORM.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'form';
  {$ENDIF}  
end;

destructor TIpHtmlNodeFORM.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeFORM.AddChild(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    if TIpHtmlNodeControl(Node).SuccessFul then
      {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}(UserData).Add(Node);
end;

{$IFNDEF HtmlWithoutHttp}
procedure TIpHtmlNodeFORM.SubmitForm;
var
  CList : {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif};
  FList,
  VList : TStringList;
  URLData: string;
  FormData: TIpFormDataEntity;

  procedure IndentifySuccessfulControls;
  begin
    EnumChildren(AddChild, CList);
  end;

  procedure BuildDataset;
  var
    i : Integer;
  begin
    for i := 0 to Pred(CList.Count) do
      with TIpHtmlNodeControl(CList[i]) do
        AddValues(FList, VList);
  end;

  procedure URLEncodeDataset;

    function Escape(const S: string): string;
    var
      i : Integer;
    begin
      Result := '';
      for i := 1 to length(S) do
        case S[i] of
        #0..#31, '+', '&', '%', '=' :
          Result := Result + '%'+IntToHex(ord(S[i]),2);
        ' ' :
          Result := Result + '+';
        else
          Result := Result + S[i];
        end;
    end;

  var
    i : Integer;
  begin
    URLData := '';
    for i := 0 to Pred(FList.Count) do begin
      if URLData <> '' then
        URLData := URLData + '&';
      URLData := URLData +
        Escape(FList[i]) +
        '=' +
        Escape(VList[i]);
    end;
  end;

  procedure MimeEncodeDataset;
  var
    i : Integer;
  begin
    FormData := TIpFormDataEntity.Create(nil);
    for i := 0 to Pred(FList.Count) do
      if copy(VList[i], 1, 7) = 'file://' then
        FormData.AddFile(copy(VList[i], 8, length(VList[i])),
          Accept, 'plain', embinary)
      else
        FormData.AddFormData(FList[i], VList[i]);
  end;

  procedure SubmitDataset;
  begin
    case Method of
    hfmGet :
      Owner.Get(Action + '?' + URLData);
    hfmPost :
      begin
        Owner.Post(Action, FormData);
        {The Formdata object will be freed by the post logic,
         which is called asynchroneously via PostMessage.
         Clear the pointer to prevent our finalization
         section from stepping on it prematurely.}
        FormData := nil;
      end;
    end;
  end;

begin
  FormData := nil;
  CList := nil;
  FList := nil;
  VList := nil;
  try
    CList := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
    FList := TStringList.Create;
    VList := TStringList.Create;
    IndentifySuccessfulControls;
    BuildDataset;
    case Method of
    hfmGet :
    {if (EncType = '') or
      (CompareText(EncType, 'application/x-www-form-urlencoded') = 0) then}
      URLEncodeDataset;
    else //hfmPost :
    {else
    if CompareText(EncType, 'multipart/form-data') = 0 then}
      MimeEncodeDataset;
    end;
    {else
      raise EIpHtmlException.Create(EncType + SHtmlEncNotSupported);}
    SubmitDataset;
  finally
    FormData.Free;
    CList.Free;
    FList.Free;
    VList.Free;
  end;
end;

procedure TIpHtmlNodeFORM.SubmitRequest;
begin
  SubmitForm;
end;
{$ENDIF}

procedure TIpHtmlNodeFORM.ResetRequest;
begin
  ResetForm;
end;

procedure TIpHtmlNodeFORM.ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    TIpHtmlNodeControl(Node).Reset;
end;

procedure TIpHtmlNodeFORM.ResetForm;
begin
  EnumChildren(ResetControl, nil);
end;

{ TIpHtmlNodeDL }

constructor TIpHtmlNodeDL.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'dl';
  {$ENDIF}
end;

procedure TIpHtmlNodeDL.Enqueue;
begin
  EnqueueElement(Owner.HardLF);
  EnqueueElement(Owner.FLIndent);
  inherited;
  EnqueueElement(Owner.FLOutdent);
end;

{ TIpHtmlNodeDT }

constructor TIpHtmlNodeDT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'dt';
  {$ENDIF}
end;

procedure TIpHtmlNodeDT.Enqueue;
begin
  inherited;
  EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodeDD }

constructor TIpHtmlNodeDD.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'dd';
  {$ENDIF}
end;

procedure TIpHtmlNodeDD.Enqueue;
begin
  EnqueueElement(Owner.HardLF);
  EnqueueElement(Owner.FLIndent);
  inherited;
  EnqueueElement(Owner.FLOutdent);
  EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodePRE }

constructor TIpHtmlNodePRE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'pre';
  {$ENDIF}
end;

destructor TIpHtmlNodePRE.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodePRE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.Preformatted := True;
  Props.FontName := Owner.FixedTypeface;
  Props.FontSize := Props.FontSize - 2;
  Props.DelayCache:=False;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodePRE.Enqueue;
begin
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodeBLOCKQUOTE }

procedure TIpHtmlNodeBLOCKQUOTE.Enqueue;
begin
  EnqueueElement(Owner.FLIndent);
  inherited;
  EnqueueElement(Owner.FLOutdent);
end;

{ TIpHtmlNodePhrase }

procedure TIpHtmlNodePhrase.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
  hpsEM :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hpsSTRONG :
    Props.FontStyle := Props.FontStyle + [fsBold];
  hpsCODE :
    Props.FontName := Owner.FixedTypeface;
  hpsKBD :
    Props.FontName := Owner.FixedTypeface;
  hpsVAR :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hpsCITE :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  end;
  
  {$IFDEF IP_LAZARUS}
  case Style of
    hpsEM      : FElementName := 'em';
    hpsSTRONG  : FElementName := 'strong';
    hpsDFN     : FElementName := 'dfn';
    hpsCODE    : FElementName := 'code';
    hpsSAMP    : FElementName := 'samp';
    hpsKBD     : FElementName := 'kbd';
    hpsVAR     : FElementName := 'var';
    hpsCITE    : FElementName := 'cite';
    hpsABBR    : FElementName := 'abbr';
    hpsACRONYM : FElementName := 'acronym';
  end;
  {$ENDIF}
end;

{ TIpHtmlNodeAPPLET }

destructor TIpHtmlNodeAPPLET.Destroy;
begin
  inherited;
  FWidth.Free;
end;

function TIpHtmlNodeAPPLET.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeAPPLET.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeBASEFONT }

procedure TIpHtmlNodeBASEFONT.ApplyProps(
  const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontSize := FONTSIZESVALUSARRAY[Size-1];
  Props.BaseFontSize := Size;
end;

{ TIpHtmlNodeINPUT }

procedure TIpHtmlNodeINPUT.SetImageGlyph(Picture: TPicture);
var
  FBitmap : TBitmap;
begin
  with TBitbtn(FControl) do begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Picture.Width;
      FBitmap.Height := Picture.Height;
      Picture.Graphic.Transparent := False;
      FBitmap.TransparentMode := tmFixed;
      FBitmap.TransparentColor := RGB(254, 254, 254);
      FBitmap.Canvas.Draw(0, 0, Picture.Graphic);
      Glyph.Assign(FBitmap);
      Width := FBitmap.Width + 4;
      Height := FBitmap.Height + 4;
    finally
      FBitmap.Free;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.Reset;
begin
  case InputType of
  hitText :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitPassword :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitCheckbox :
    begin
      with TCheckBox(FControl) do
        Checked := Self.Checked;
    end;
  hitRadio :
    begin
{$IFDEF VERSION3ONLY}
      with FControl do
{$ELSE}
      with THtmlRadioButton(FControl) do
{$ENDIF}
        Checked := Self.Checked;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.CreateControl(Parent: TWinControl);
var
  iCurFontSize: integer;
  aCanvas : TCanvas;

  function OwnerForm: TIpHtmlNode;
  begin
    Result := FParentNode;
    while (Result <> nil) and not (Result is TIpHtmlNodeFORM) do
      Result := Result.FParentNode;
  end;

  procedure setCommonProperties;
  begin
      FControl.Visible := False;
      FControl.Parent := Parent;
      adjustFromCss;
      aCanvas.Font.Size := FControl.Font.Size;
  end;

  procedure setWidhtHeight(iSize, iTopPlus, iSidePlus: integer);
  begin
        if iSize <> -1 then
          FControl.Width := iSize * aCanvas.TextWidth('0') + iSidePlus
        else
          FControl.Width := 20 * aCanvas.TextWidth('0')  + iSidePlus;
        FControl.Height := aCanvas.TextHeight('Wy') + iTopPlus;
  end;

begin
  Owner.ControlCreate(Self);
  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  case InputType of
  hitText :
    begin
      FControl := TEdit.Create(Parent);
      setCommonProperties;
      with TEdit(FControl) do begin
        Text := Value;
        MaxLength := Self.MaxLength;
        setWidhtHeight(Self.Size, 8, 0);
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        OnChange := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitPassword :
    begin
      FControl := TEdit.Create(Parent);
      setCommonProperties;
      with TEdit(FControl) do begin
        Text := Value;
        MaxLength := Self.MaxLength;
        setWidhtHeight(1, 8, 0);
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        PasswordChar := '*';
        OnChange := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitCheckbox :
    begin
      FControl := TCheckBox.Create(Parent);
      setCommonProperties;
      with TCheckBox(FControl) do begin
        setWidhtHeight(1, 8, 0);
        //Width := 13;
        //if Height < 13 then Height := 13;
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitRadio :
    begin
{$IFDEF VERSION3ONLY}
      FControl := TRadioButton.Create(Parent);
{$ELSE}
      FControl := THtmlRadioButton.Create(Parent);
{$ENDIF}
      FControl.Tag := PtrInt(OwnerForm);
      setCommonProperties;
{$IFDEF VERSION3ONLY}
      with TRadioButton(FControl) do begin
{$ELSE}
      with THtmlRadioButton(FControl) do begin
{$ENDIF}
        setWidhtHeight(1, 8, 0);
        //Width := 13;
        //if Height < 13 then Height := 13;
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitSubmit :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefSubmitCaption;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := SubmitClick;
      end;
    end;
  hitReset :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefResetCaption;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ResetClick;
      end;
    end;
  hitFile :
    begin
      FControl := TPanel.Create(Parent);
      setCommonProperties;
      with TPanel(FControl) do begin
        Width := 200;
        Height := aCanvas.TextHeight('Wy') + 12;
        Enabled := not Self.Disabled and not Self.Readonly;
        BevelInner := bvNone;
        BevelOuter := bvNone;
        BorderStyle := bsNone;
      end;
      FFileSelect := TButton.Create(Parent);
      with FFileSelect do begin
        Parent := FControl;
        Height := aCanvas.TextHeight(SHtmlDefBrowseCaption) + 10;
        Width := aCanvas.TextWidth(SHtmlDefBrowseCaption) + 40;
        Left := FControl.Left + FControl.Width - Width;
        Top := 1;
        Caption := SHtmlDefBrowseCaption;
        OnClick := FileSelect;
      end;
      FFileEdit := TEdit.Create(Parent);
      with FFileEdit do begin
        Parent := FControl;
        Left := 1;
        Top := 1;
        Width := FControl.Width - FFileSelect.Width;
        Height := FControl.Height - 2;
      end;
    end;
  hitHidden :
    begin
    end;
  hitImage :
    begin
      FControl := TBitbtn.Create(Parent);
      setCommonProperties;
      Owner.DoGetImage(Self, Owner.BuildPath(Src), FPicture);
      if FPicture = nil
        then FPicture := Owner.DefaultImage;
      with TBitbtn(FControl) do begin
        Caption := Self.Value;
        Enabled := not Self.Disabled and not Self.Readonly;
        SetImageGlyph(FPicture);
      end;
    end;
  hitButton :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        Caption := Self.Value;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
      end;
    end;
  end;
  if FControl <> nil then
  begin
    FControl.Hint := Alt;
    FControl.ShowHint:=True;
  end;
  aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeINPUT.Draw;
begin
{
  if Assigned(FInlineCSSProps) then
  begin
       if FInlineCSSProps.BGColor <> -1 then FControl.Color := FInlineCSSProps.BGColor;
       if FInlineCSSProps.Color <> -1 then FControl.Font.Color := FInlineCSSProps.Color;
       if FInlineCSSProps.Font.Size <> '' then FControl.Font.size := GetFontSizeFromCSS(FControl.Font.size, FInlineCSSProps.Font.Size);
  end;
}
  inherited;
{$IFDEF VERSION3ONLY}
  if FControl is TRadioButton then begin
{$ELSE}
  if FControl is THtmlRadioButton then begin
{$ENDIF}
    if Props.BgColor <> -1 then
{$IFDEF VERSION3ONLY}
      TRadioButton(FControl).Color := Props.BgColor;
{$ELSE}
      THtmlRadioButton(FControl).Color := Props.BgColor;
{$ENDIF}
  end;
end;

procedure TIpHtmlNodeINPUT.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  if FPicture <> Owner.DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  SetImageGlyph(FPicture);
  InvalidateSize;
end;

procedure TIpHtmlNodeINPUT.AddValues(NameList, ValueList : TStringList);
var
  S : string;
begin
  S := '';
  case InputType of
  hitText,
  hitPassword :
    S := TEdit(FControl).Text;
  hitCheckbox :
    S := Value;
  hitRadio :
    S := Value;
  hitFile :
    S := 'file://'+FFileEdit.Text;
  hitHidden :
    S := FValue;
  end;
  if S <> '' then begin
    NameList.Add(Name);
    ValueList.Add(S);
  end;
end;

function TIpHtmlNodeINPUT.Successful: Boolean;
begin
  Result :=
    (Name <> '')and
    ( (InputType = hitHidden) or
      ( (not Disabled) and
        (InputType in [hitText, hitPassword, hitCheckbox, hitRadio , hitFile])
      )
    );
  if Result then begin
    case InputType of
    hitText,
    hitPassword :
      Result := TEdit(FControl).Text <> '';
    hitCheckbox :
      Result := TCheckBox(FControl).Checked;
    hitRadio :
{$IFDEF VERSION3ONLY}
      Result := TRadioButton(FControl).Checked;
{$ELSE}
      Result := THtmlRadioButton(FControl).Checked;
{$ENDIF}
    hitFile :
      Result := FFileEdit.Text <> '';
    hitHidden :
      Result := FValue <> '';
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.SubmitClick(Sender: TObject);
var
  vCancel: boolean;
begin
  vCancel := False;
  Owner.ControlClick2(Self, vCancel);
  if not vCancel then SubmitRequest;
end;

procedure TIpHtmlNodeINPUT.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeINPUT.getControlValue;
begin
  case InputType of
  hitText,
  hitPassword :
    Value := TEdit(FControl).Text;
  hitCheckbox :
    Checked := TCheckBox(FControl).Checked;
  hitRadio :
{$IFDEF VERSION3ONLY}
    Checked := TRadioButton(FControl).Checked;
{$ELSE}
    Checked := THtmlRadioButton(FControl).Checked;
{$ENDIF}
  end;
end;

procedure TIpHtmlNodeINPUT.ButtonClick(Sender: TObject);
begin
  getControlValue;
  Owner.ControlClick(Self);
end;

procedure TIpHtmlNodeINPUT.ControlOnEditingDone(Sender: TObject);
begin
  getControlValue;
  Owner.ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeINPUT.ControlOnChange(Sender: TObject);
begin
  getControlValue;
  Owner.ControlOnChange(Self);
end;

function TIpHtmlNodeINPUT.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeINPUT.FileSelect(Sender: TObject);
begin
  with TOpenDialog.Create(FControl) do
    try
      if Execute then
        FFileEdit.Text := FileName;
    finally
      free;
    end;
end;

constructor TIpHtmlNodeINPUT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'input';
  {$ENDIF}
end;

destructor TIpHtmlNodeINPUT.Destroy;
begin
  inherited;
  FPicture.Free;
end;

{ TIpHtmlNodeSELECT }

constructor TIpHtmlNodeSELECT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'select';
  {$ENDIF}
  FWidth := -1;
  FSize := -1;
end;

destructor TIpHtmlNodeSELECT.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeSELECT.AddValues(NameList, ValueList : TStringList);
var
  i : Integer;
begin
  if FControl is TListBox then
    with TListBox(FControl) do begin
      for i := 0 to Pred(Items.Count) do
        if Selected[i] then begin
          NameList.Add(Self.Name);
          ValueList.Add(Items[i]);
        end;
    end
  else with TComboBox(FControl) do begin
    NameList.Add(Self.Name);
    ValueList.Add(Items[ItemIndex]);
  end;
end;

procedure TIpHtmlNodeSELECT.CreateControl(Parent: TWinControl);
var
  i, j, k, MinW : Integer;
  S, SelectedText : string;
  B : PAnsiChar;
  iCurFontSize: integer;
  aCanvas : TCanvas;
begin
  Owner.ControlCreate(Self);
  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  i := Size;
  if i = -1 then i:= 1;

  if Self.Multiple then begin
    FControl := TListBox.Create(Parent);
    FControl.Visible := False;
    FControl.Parent := Parent;
    adjustFromCss;
    with TListBox(FControl) do begin
      IntegralHeight := True;
      Height := (4 + aCAnvas.TextHeight('Wy')) * i;
      MultiSelect := True;
      Enabled := not Self.Disabled;
      OnClick := ButtonClick;
      OnSelectionChange := ListBoxSelectionChange;
    end;
  end else begin
    FControl := TComboBox.Create(Parent);
    FControl.Visible := False;
    FControl.Parent := Parent;
    adjustFromCss;
    with TComboBox(FControl) do begin
      Style := csDropDownList;
      Height := (4 + aCAnvas.TextHeight('Wy')) * i;
      Enabled := not Self.Disabled;
      ReadOnly := not Self.ComboBox;
      OnClick := ButtonClick;
      OnEditingDone := ControlOnEditingdone;
    end;
  end;
  MinW := 50;
  SelectedText := '';
  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeOPTION then
      with TIpHtmlNodeOPTION(FChildren[i]) do begin
        if (FChildren.Count > 0)
        and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
          S := TIpHtmlNodeText(FChildren[0]).EscapedText;
          Getmem(B, length(S) + 1);
          try
            TrimFormatting(S, B);
            S := Trim(B);
            if Self.Multiple then begin
              j := TListBox(FControl).Items.Add(S);
              MinW := MaxI2(MinW, aCanvas.TextWidth(S));
              TListBox(FControl).Selected[j] := Selected;
            end else begin
              TComboBox(FControl).Items.Add(S);
              MinW := MaxI2(MinW, aCanvas.TextWidth(S));
              if Selected then
                SelectedText := S;
            end;
          finally
            FreeMem(B);
          end;
        end;
      end
    else
    if TObject(FChildren[i]) is TIpHtmlNodeOPTGROUP then
      with TIpHtmlNodeOPTGROUP(FChildren[i]) do begin
        for j := 0 to Pred(FChildren.Count) do
          if TObject(FChildren[j]) is TIpHtmlNodeOPTION then
            with TIpHtmlNodeOPTION(FChildren[j]) do begin
              if (FChildren.Count > 0)
              and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
                S := TIpHtmlNodeText(FChildren[0]).EscapedText;
                GetMem(B, length(S) + 1);
                try
                  TrimFormatting(S, B);
                  S := Trim(B);
                  if Self.Multiple then begin
                    k := TListBox(FControl).Items.Add(S);
                    MinW := MaxI2(MinW, aCanvas.TextWidth(S));
                    TListBox(FControl).Selected[k] := Selected;
                  end else begin
                    TComboBox(FControl).Items.Add(Trim(B));
                    MinW := MaxI2(MinW, aCanvas.TextWidth(S));
                    if Selected then
                      SelectedText := S;
                  end;
                finally
                  FreeMem(B);
                end;
              end;
            end;
      end;
  if SelectedText <> '' then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
  if FComboBox and (Width <> -1) then FControl.Width := Width*aCanvas.TextWidth('0')+ 20
  else FControl.Width := MinW + 40;
  FControl.ShowHint:=True;
  FControl.Hint:= Alt;
  aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeSELECT.Reset;
var
  i, j, k : Integer;
  S, SelectedText : string;
  B : PAnsiChar;
begin
  SelectedText := '';
  if Self.Multiple then
    TListBox(FControl).Clear
  else
    TComboBox(FControl).Clear;
  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeOPTION then
      with TIpHtmlNodeOPTION(FChildren[i]) do begin
        if (FChildren.Count > 0)
        and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
          S := TIpHtmlNodeText(FChildren[0]).EscapedText;
          GetMem(B, length(S) + 1);
          try
            TrimFormatting(S, B);
            if Self.Multiple then begin
              j := TListBox(FControl).Items.Add(Trim(B));
              TListBox(FControl).Selected[j] := Selected;
            end else begin
              TComboBox(FControl).Items.Add(Trim(B));
              if Selected then
                SelectedText := Trim(B);
            end;
          finally
            FreeMem(B);
          end;
        end;
      end
    else
    if TObject(FChildren[i]) is TIpHtmlNodeOPTGROUP then
      with TIpHtmlNodeOPTGROUP(FChildren[i]) do begin
        for j := 0 to Pred(FChildren.Count) do
          if TObject(FChildren[j]) is TIpHtmlNodeOPTION then
            with TIpHtmlNodeOPTION(FChildren[j]) do begin
              if (FChildren.Count > 0)
              and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
                S := TIpHtmlNodeText(FChildren[0]).EscapedText;
                GetMem(B, length(S) + 1);
                try
                  TrimFormatting(S, B);
                  if Self.Multiple then begin
                    k := TListBox(FControl).Items.Add(Trim(B));
                    TListBox(FControl).Selected[k] := Selected;
                  end else begin
                    TComboBox(FControl).Items.Add(Trim(B));
                    if Selected then
                      SelectedText := Trim(B);
                  end;
                finally
                  FreeMem(B);
                end;
              end;
            end;
      end;
  if not Self.Multiple and (SelectedText <> '') then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
end;

function TIpHtmlNodeSELECT.Successful: Boolean;
begin
  Result := (Name <> '') and not Disabled;
  if Result then
    if FControl is TListBox then
      Result := TListBox(FControl).SelCount > 0
    else
      Result := TComboBox(FControl).ItemIndex <> -1;
end;

procedure TIpHtmlNodeSELECT.ButtonClick(Sender: TObject);
begin
  Owner.ControlClick(Self);
end;

procedure TIpHtmlNodeSELECT.ControlOnEditingDone(Sender: TObject);
begin
  Owner.ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeSELECT.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  Owner.ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeSELECT.setText(aText: string);
begin
  if FComboBox then TComboBox(FControl).Text := aText;
end;

function TIpHtmlNodeSELECT.getText: string;
begin
  if FComboBox then
    result := TComboBox(FControl).Text
  else if FMultiple then
    result := IntToStr(TComboBox(FControl).ItemIndex)
  else
    result := IntToStr(TComboBox(FControl).ItemIndex);
end;

{ TIpHtmlNodeTEXTAREA }

constructor TIpHtmlNodeTEXTAREA.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'textarea';
  {$ENDIF}
end;

destructor TIpHtmlNodeTEXTAREA.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeTEXTAREA.AddValues(NameList,
  ValueList: TStringList);
begin
  NameList.Add(Name);
  ValueList.Add(TMemo(FControl).Text);
end;

procedure TIpHtmlNodeTEXTAREA.CreateControl(Parent: TWinControl);
var
  i : Integer;
  S : string;
  B : PAnsiChar;
  iCurFontSize: integer;
  aCanvas : TCanvas;
begin
  Owner.ControlCreate(Self);
  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  FControl := TMemo.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  TMemo(FControl).OnEditingDone:= ControlOnEditingDone;
  adjustFromCss;

  with TMemo(FControl) do begin
    Width := Cols * TFriendPanel(Parent).Canvas.TextWidth('0'); 
    Height := Rows * TFriendPanel(Parent).Canvas.TextHeight('Wy');
    Enabled := not Self.Disabled;
  end;

  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(FChildren[i]).EscapedText;
      Getmem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        FreeMem(B);
      end;
    end;
    aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeTEXTAREA.Reset;
var
  i : Integer;
  S : string;
  B : PAnsiChar;
begin
  TMemo(FControl).Clear;
  for i := 0 to Pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(FChildren[i]).EscapedText;
      GetMem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        Freemem(B);
      end;
    end;
end;

function TIpHtmlNodeTEXTAREA.Successful: Boolean;
begin
  Result := trim(TMemo(FControl).Text) <> '';
end;

procedure TIpHtmlNodeTEXTAREA.ControlOnEditingDone(Sender: TObject);
begin
  Owner.ControlOnEditingDone(Self);
end;

{ TIpHtmlNodeHtml }

procedure TIpHtmlNodeHtml.CalcMinMaxHtmlWidth(const RenderProps: TIpHtmlProps; var Min, Max: Integer);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).CalcMinMaxPropWidth(RenderProps, Min, Max);
end;

function TIpHtmlNodeHtml.GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      Result := TIpHtmlNodeBody(FChildren[i]).GetHeight(RenderProps, Width);
end;

function TIpHtmlNodeHtml.HasBodyNode : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FChildren.Count - 1 do begin
    Result := (TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody);
    if Result then
      Break;
  end;
end;

procedure TIpHtmlNodeHtml.Layout(const RenderProps: TIpHtmlProps; const TargetRect: TRect);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).Layout(RenderProps, TargetRect);
end;

procedure TIpHtmlNodeHtml.Render(const RenderProps: TIpHtmlProps);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).Render(RenderProps);
end;

{ TIpHtmlNodeCore }

procedure TIpHtmlNodeCore.ParseBaseProps(aOwner : TIpHtml);
{$IFDEF IP_LAZARUS}
var
  Commands: TStringList;
{$ENDIF}
begin
  with aOwner do begin
    Id := FindAttribute(htmlAttrID);
    ClassId := FindAttribute(htmlAttrCLASS);
    Title := FindAttribute(htmlAttrTITLE);
    Style := FindAttribute(htmlAttrSTYLE);
  end;
  {$IFDEF IP_LAZARUS}
  if Style <> '' then
  begin
    if InlineCSS = nil then
      InlineCSS := TCSSProps.Create;
    Commands := SeperateCommands(Style);
    InlineCSS.ReadCommands(Commands);
    Commands.Free;
  end;
  {$ENDIF}
end;

{$IFDEF IP_LAZARUS}
(* look up the props for all CSS selectors that directly match this node, merge
   them all into one object (FCombinedCSSProps) and then apply them to Props.
   When FCombinedCSSProps already exists then the expensive lookup is skipped
   and the existing object is used. *)
procedure TIpHtmlNodeCore.LoadAndApplyCSSProps;
var
  TmpProps: TCSSProps;

begin
  if Owner.CSS = nil then
    exit;

  if FCombinedCSSProps = nil then
  begin
    FCombinedCSSProps := TCSSProps.Create;

    // first look for tag name only
    TmpProps := Owner.CSS.GetPropsObject(ElementName);
    if TmpProps <> nil then
      FCombinedCSSProps.MergeAdditionalProps(TmpProps);

    // look for .class if there is one
    if ClassID <> '' then
    begin
      TmpProps := Owner.CSS.GetPropsObject('', ClassId);
      if TmpProps <> nil then
        FCombinedCSSProps.MergeAdditionalProps(TmpProps);

      // then look for a tag.class selector if there is one
      TmpProps := Owner.CSS.GetPropsObject(ElementName, ClassId);
      if TmpProps <> nil then
        FCombinedCSSProps.MergeAdditionalProps(TmpProps);
    end;

    // lookup props for an id selector
    TmpProps := Owner.CSS.GetPropsObject(Id);
    if TmpProps <> nil then
      FCombinedCSSProps.MergeAdditionalProps(TmpProps);

    // inline css, not from the stylesheet
    if InlineCSS <> nil then
      FCombinedCSSProps.MergeAdditionalProps(InlineCSS);

  end;

  // look for :hover styles...
  if not FHoverPropsLookupDone then
  begin
    FHoverPropsRef := Owner.CSS.GetPropsObject(ElementName + ':hover');
    FHoverPropsLookupDone := True;
  end;
  // ...apply them if there are any.
  if FHoverPropsRef <> nil then
  begin
    Props.DelayCache:=True;
    if FHoverPropsRef.Color <> -1 then
      Props.HoverColor := FHoverPropsRef.Color;
    if FHoverPropsRef.BgColor <> -1 then
      Props.HoverBgColor := FHoverPropsRef.BgColor;
    Props.DelayCache:=False;
  end;

  Props.DelayCache:=True;
  ApplyCSSProps(FCombinedCSSProps, Props);
  Props.DelayCache:=False;
end;

function TIpHtmlNodeCore.SelectCSSFont(const aFont: string): string;
begin
  // todo: implement font matching
  result := FirstString(aFont);
end;

procedure TIpHtmlNodeCore.ApplyCSSProps(const ACSSProps: TCSSProps;
  const props: TIpHtmlProps);

  function CssMarginToProps(CssMargin: TCSSMargin;
    out ElemMargin: TIpHtmlElemMargin): boolean;
  begin
    ElemMargin.Style:=hemsAuto;
    ElemMargin.Size:=0;
    if CssMargin.Style=cmsNone then exit(false);
    if CssMargin.Style=cmsAuto then exit(true);
    if CssMargin.Style=cmsPx then begin
      ElemMargin.Style:=hemsPx;
      ElemMargin.Size:=CssMargin.Size;
      exit(true);
    end;
    if CssMargin.Style=cmsEm then begin
      ElemMargin.Style:=hemsPx;
      ElemMargin.Size:=10*CssMargin.Size; // 1em = 1 current font size
      exit(true);
    end;
    debugln(['TIpHtmlNodeCore.ApplyCSSProps.CssMarginToProps note: margin style not supported ',ord(CssMargin.Style)]);
  end;

var
  ElemMargin: TIpHtmlElemMargin;
begin
  if (ACSSProps<>nil) and (props<>nil) then
  begin
    props.DelayCache:=True;
    {$WARNING Setting these font colors and name messes up the alignment for some reason}
    if ACSSProps.Color <> -1 then begin
      Props.FontColor := ACSSProps.Color;
    end;

    if ACSSProps.BGColor <> -1 then begin
      Props.BgColor := ACSSProps.BGColor;
    end;

    if ACSSProps.Alignment <> haUnknown then begin
      Props.Alignment := ACSSProps.Alignment;
    end;

    if ACSSProps.Font.Name <> '' then begin
      // put the code here, later refactore it
      Props.FontName := SelectCSSFont(ACSSProps.Font.Name);
    end;

     {$WARNING TODO Set Font size from CSS Value}
    // see http://xhtml.com/en/CSS/reference/font-size/
    if ACSSProps.Font.Size <> '' then begin
      // Props.FontSize :=  ACSSProps.Font.Size;
      props.FontSize:=GetFontSizeFromCSS(Props.FontSize, ACSSProps.Font.Size);
    end;

    if ACSSProps.Font.Style <> cfsNormal then begin
      case ACSSProps.Font.Style of
        cfsItalic,cfsOblique: Props.FontStyle := Props.FontStyle + [fsItalic];
        cfsInherit: ; // what to do?: search through parent nodes looking for a computed value
      end;
    end;

    if ACSSProps.Font.Weight <> cfwNormal then begin
      case ACSSProps.Font.Weight of
        cfwBold    : Props.FontStyle := Props.FontStyle + [fsBold];
        cfwBolder  : Props.FontStyle := Props.FontStyle + [fsBold];
        cfwLighter : Props.FontStyle := Props.FontStyle - [fsBold];
        cfw100     : ;
        cfw200     : ;
        cfw300     : ;
        cfw400     : ;
        cfw500     : ;
        cfw600     : ;
        cfw700     : ;
        cfw800     : ;
        cfw900     : ;
      end;
    end;

    if CssMarginToProps(ACSSProps.MarginTop,ElemMargin) then
      props.ElemMarginTop:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginRight,ElemMargin) then
      props.ElemMarginRight:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginBottom,ElemMargin) then
      props.ElemMarginBottom:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginLeft,ElemMargin) then
      props.ElemMarginLeft:=ElemMargin;

    props.DelayCache:=False;
  end;
end;

function TIpHtmlNodeCore.ElementName: String;
begin
  Result := FElementName;
end;

function TIpHtmlNodeCore.GetFontSizeFromCSS(CurrentFontSize:Integer;
  aFontSize: string):Integer;

  function GetFSize(aUnits: string): double;
  var
    i: Integer;
  begin
    i := pos(aUnits, aFontSize);
    if i>0 then
      result := StrToFloatDef(copy(aFontSize,1,i-1), -1.0)
    else
      result := -1.0;
  end;
  
  function GetParentFontSize: integer;
  begin
    if (FParentNode is TIpHtmlNodeBlock) then
      result :=TIpHtmlNodeBlock(FParentNode).Props.FontSize
    else
    if (FParentNode is TIpHtmlNodeGenInline) then
      result := TIpHtmlNodeGenInline(FparentNode).Props.FontSize
    else
      result := CurrentFontSize;
  end;
  
var
  P: double;
  //ParentFSize: Integer;
begin
  result := CurrentFontSize;

  // check pt
  P:=GetFSize('pt');
  if P>0 then begin
    result := round(P);
    exit;
  end;
  
  // check px
  P:=GetFSize('px');
  if P>0 then begin
    // calculate points based on screen resolution :(
    // at 96dpi CSS21 recommneds 1px=0.26 mm
    // TODO: use screen resolution, check printing!
    Result := Round(P*0.7370241);
    exit;
  end;

  //todo: em, ex are supposed to be based on the computed pixel size of
  //      parent node, tpipro has no provision for this....

  // check %
  P:=GetFSize('%');
  if P>0 then begin
    result := round(GetParentFontSize * P/100);
    exit;
  end;
  
  // check em
  P:=GetFSize('em');
  if P>0 then begin
    result := round(GetParentFontSize * P);
  end;
end;

destructor TIpHtmlNodeCore.Destroy;
begin
  if Assigned(FInlineCSSProps) then
    FInlineCSSProps.Free;
  if Assigned(FCombinedCSSProps) then
    FCombinedCSSProps.Free;
  inherited Destroy;
end;
{$ENDIF}

{ TIpHtmlNodeINS }

procedure TIpHtmlNodeINS.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsUnderline];
end;

{ TIpHtmlNodeDEL }

procedure TIpHtmlNodeDEL.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsStrikeOut];
end;

{ TIpHtmlNodeTHEAD }

constructor TIpHtmlNodeTHEAD.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'thead';
  {$ENDIF}
  FVAlign := hva3Middle;
end;

{ TIpHtmlNodeTBODY }

constructor TIpHtmlNodeTBODY.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  {$IFDEF IP_LAZARUS}
  FElementName := 'tbody';
  {$ENDIF}
  FVAlign := hva3Middle;
end;

{ TIpHtmlNodeSTYLE }

function TIpHtmlNodeSTYLE.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNodeSTYLE.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

{ TIpHtmlNodeIFRAME }

procedure TIpHtmlNodeIFRAME.CreateControl(Parent: TWinControl);
begin
  Owner.ControlCreate(Self);
  Owner.CreateIFrame(Parent, Self, FControl);
end;

procedure TIpHtmlNodeIFRAME.AddValues(NameList, ValueList: TStringList);
begin
end;

procedure TIpHtmlNodeIFRAME.Reset;
begin
end;

function TIpHtmlNodeIFRAME.Successful: Boolean;
begin
  Result := False;
end;

destructor TIpHtmlNodeIFRAME.Destroy;
begin
  inherited;
  FHeight.Free;
  FWidth.Free;
end;

procedure TIpHtmlNodeIFRAME.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIFRAME.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetScrolling(
  const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeBUTTON }

procedure TIpHtmlNodeBUTTON.AddValues(NameList, ValueList : TStringList);
begin
end;

constructor TIpHtmlNodeBUTTON.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'button';
  {$ENDIF}
  Owner.FControlList.Add(Self);
end;

destructor TIpHtmlNodeBUTTON.Destroy;
begin
  Owner.FControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeBUTTON.CreateControl(Parent: TWinControl);
var
   iCurFontSize: integer;
   aCanvas : TCanvas;
begin
  Owner.ControlCreate(Self);
  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  FControl := TButton.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  adjustFromCss;

  with TButton(FControl) do begin
    Enabled := not Self.Disabled;
    Caption := Value;
    case ButtonType of
    hbtSubmit :
      begin
        OnClick := SubmitClick;
        if Caption = '' then
          Caption := SHtmlDefSubmitCaption;
      end;
    hbtReset :
      begin
        OnClick := ResetClick;
        if Caption = '' then
          Caption := SHtmlDefResetCaption;
      end;
    hbtButton :
      begin
        OnClick := ButtonClick;
      end;
    end;
    Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
    Height := TFriendPanel(Parent).Canvas.TextHeight(Caption) + 10;
  end;
  aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeBUTTON.Reset;
begin
end;

procedure TIpHtmlNodeBUTTON.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeBUTTON.SubmitClick(Sender: TObject);
begin
  SubmitRequest;
end;

procedure TIpHtmlNodeBUTTON.ButtonClick(Sender: TObject);
begin
  Owner.ControlClick(Self);
end;

function TIpHtmlNodeBUTTON.Successful: Boolean;
begin
  Result := False;
end;

{ TIpHtmlNodeCOL }

destructor TIpHtmlNodeCOL.Destroy;
begin
  inherited;
  FWidth.Free;
end;

{ TIpHtmlNodeCOLGROUP }

destructor TIpHtmlNodeCOLGROUP.Destroy;
begin
  inherited;
  FWidth.Free;
end;

{ TIpHtmlNodeLABEL }

constructor TIpHtmlNodeLABEL.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.FControlList.Add(Self);
end;

destructor TIpHtmlNodeLABEL.Destroy;
begin
  Owner.FControlList.Remove(Self);
  inherited;
end;

{ TIpHtmlNodeNOBR }

procedure TIpHtmlNodeNOBR.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := True;
end;

{ TIpHtmlProps }

function TIpHtmlProps.AIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result :=
    (PropA = Compare.PropA);
end;

procedure TIpHtmlProps.Assign(Source: TIpHtmlProps);
begin
  if PropA <> Source.PropA then begin
    PropA.DecUse;
    PropA := Source.PropA;
    PropA.IncUse;
  end;
  if PropB <> Source.PropB then begin
    PropB.DecUse;
    PropB := Source.PropB;
    PropB.IncUse;
  end;
end;

function TIpHtmlProps.BIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result :=
    (PropB = Compare.PropB);
end;

constructor TIpHtmlProps.Create(Owner: TIpHtml);
begin
  FOwner := Owner;
  PropA := Owner.DummyA;
  PropA.IncUse;
  PropB := Owner.DummyB;
  PropB.IncUse;
  //BgColor := -1;
end;

destructor TIpHtmlProps.Destroy;
begin
  PropA.DecUse;
  PropB.DecUse;
  inherited;
end;

function TIpHtmlProps.GetAlignment: TIpHtmlAlign;
begin
  Result := PropB.Alignment;
end;

function TIpHtmlProps.GetALinkColor: TColor;
begin
  Result := PropB.ALinkColor;
end;

function TIpHtmlProps.GetBaseFontSize: Integer;
begin
  Result := PropA.BaseFontSize;
end;

function TIpHtmlProps.GetBgColor: TColor;
begin
  Result := PropB.BgColor;
end;

function TIpHtmlProps.GetElemMarginBottom: TIpHtmlElemMargin;
begin
  Result:=PropB.ElemMarginBottom;
end;

function TIpHtmlProps.GetElemMarginLeft: TIpHtmlElemMargin;
begin
  Result:=PropB.ElemMarginLeft;
end;

function TIpHtmlProps.GetElemMarginRight: TIpHtmlElemMargin;
begin
  Result:=PropB.ElemMarginRight;
end;

function TIpHtmlProps.GetElemMarginTop: TIpHtmlElemMargin;
begin
  Result:=PropB.ElemMarginTop;
end;

function TIpHtmlProps.GetFontBaseline: Integer;
begin
  Result := PropB.FontBaseline;
end;

function TIpHtmlProps.GetFontColor: TColor;
begin
  Result := PropB.FontColor;
end;

function TIpHtmlProps.GetFontName: string;
begin
  Result := PropA.FontName;
end;

function TIpHtmlProps.GetFontSize: Integer;
begin
  Result := PropA.FontSize;
end;

function TIpHtmlProps.GetFontStyle: TFontStyles;
begin
  Result := PropA.FontStyle;
end;

function TIpHtmlProps.GetLinkColor: TColor;
begin
  Result := PropB.LinkColor;
end;

function TIpHtmlProps.GetNoBreak: Boolean;
begin
  Result := PropB.NoBreak;
end;

function TIpHtmlProps.GetPreformatted: Boolean;
begin
  Result := PropB.Preformatted;
end;

function TIpHtmlProps.GetVAlignment: TIpHtmlVAlign3;
begin
  Result := PropB.VAlignment;
end;

function TIpHtmlProps.GetVLinkColor: TColor;
begin
  Result := PropB.VLinkColor;
end;

function TIpHtmlProps.GetHoverColor: TColor;
begin
  Result := PropB.HoverColor;
end;

function TIpHtmlProps.GetHoverBgColor: TColor;
begin
  Result := PropB.HoverBgColor;
end;

function TIpHtmlProps.IsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result := (PropA = Compare.PropA) and (PropB = Compare.PropB);
end;

function TIpHtml.FindPropARec(var pRec: TIpHtmlPropAFieldsRec): TIpHtmlPropA;
var
  i: Integer;
begin
  for i := 0 to Pred(PropACache.Count) do begin
    Result := TIpHtmlPropA(PropACache[i]);
    if CompareByte(Result.FPropRec, pRec, sizeof(TIpHtmlPropAFieldsRec)) = 0 then
       exit;
  end;
  Result := nil;
end;

procedure TIpHtml.DelDuplicatePropA(aProp: TIpHtmlPropA);
var
  i: Integer;
  vProp : TIpHtmlPropA;
begin
  for i := Pred(PropACache.Count) downto 0 do begin
    vProp := TIpHtmlPropA(PropACache[i]);
    if CompareByte(vProp.FPropRec, aProp.FPropRec, sizeof(TIpHtmlPropAFieldsRec)) = 0 then
       if vProp <> aProp then
       begin
         PropACache.Delete(i);
         exit;
       end;
  end;
end;

function TIpHtml.FindPropBRec(var pRec: TIpHtmlPropBFieldsRec): TIpHtmlPropB;
var
  i: Integer;
begin
  for i := 0 to Pred(PropBCache.Count) do begin
    Result := TIpHtmlPropB(PropBCache[i]);
    if CompareByte(Result.FPropRec, pRec, sizeof(TIpHtmlPropBFieldsRec)) = 0 then
       exit;
  end;
  Result := nil;
end;

procedure TIpHtml.DelDuplicatePropB(aProp: TIpHtmlPropB);
var
  i: Integer;
  vProp : TIpHtmlPropB;
begin
  for i := Pred(PropBCache.Count) downto 0 do begin
    vProp := TIpHtmlPropB(PropBCache[i]);
    if CompareByte(vProp.FPropRec, aProp.FPropRec, sizeof(TIpHtmlPropBFieldsRec)) = 0 then
       if vProp <> aProp then
       begin
            PropBCache.Delete(i);
            exit;
       end;
  end;
end;

procedure TIpHtmlProps.CommitCache;
begin
  if FDelayCache > 0 then
  begin
    FDelayCache := 1;
    SetDelayCache(false);
  end;
end;

function TIpHtmlProps.GetDelayCache: boolean;
begin
  result := FDelayCache > 0;
end;

procedure TIpHtmlProps.SetDelayCache(b: boolean);
begin
  if b then Inc(FDelayCache)
  else if FDelayCache > 0 then
    Dec(FDelayCache);

  if (not b) and (FDelayCache = 0) then
  begin
      if FDirtyA then
      begin
        //Finish/Commit transaction
        FDirtyA := False;
      end;
      if FDirtyB then
      begin
        //Finish/Commit transaction
        FDirtyB := False;
      end;
  end;
end;

procedure TIpHtmlProps.CopyPropARecTo(var pRec: TIpHtmlPropAFieldsRec);
begin
    Move(PropA.FPropRec, pRec, sizeof(TIpHtmlPropAFieldsRec))
end;

procedure TIpHtmlProps.CopyPropBRecTo(var pRec: TIpHtmlPropBFieldsRec);
begin
    Move(PropB.FPropRec, pRec, sizeof(TIpHtmlPropBFieldsRec))
end;

procedure TIpHtmlProps.CopyPropARecFrom(var pRec: TIpHtmlPropAFieldsRec);
begin
    Move(pRec, PropA.FPropRec, sizeof(TIpHtmlPropAFieldsRec));
end;

procedure TIpHtmlProps.CopyPropBRecFrom(var pRec: TIpHtmlPropBFieldsRec);
begin
    Move(pRec, PropB.FPropRec, sizeof(TIpHtmlPropBFieldsRec));
end;

procedure TIpHtmlProps.FindOrCreatePropA(var pRec: TIpHtmlPropAFieldsRec);
var
  NewPropA : TIpHtmlPropA;
begin
  if FDirtyA then
    //we are in a transaction updating a new unique entry
    CopyPropARecFrom(pRec)
  else
  begin
    NewPropA := FOwner.FindPropARec(pRec);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.Create;
      Move(pRec, NewPropA.FPropRec, sizeof(TIpHtmlPropAFieldsRec));
      //Start Transaction if DelayCache is set
      if DelayCache then FDirtyA := True;
      FOwner.PropACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    PropA.DecUse;
    PropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.FindOrCreatePropB(var pRec: TIpHtmlPropBFieldsRec);
var
  NewPropB : TIpHtmlPropB;
begin
  if FDirtyB then
    //we are in a transaction updating a new unique entry
    CopyPropBRecFrom(pRec)
  else
  begin
    NewPropB := FOwner.FindPropBRec(pRec);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.Create(FOwner);
      Move(pRec, NewPropB.FPropRec, sizeof(TIpHtmlPropBFieldsRec));
      //Start Transaction if DelayCache is set
      if DelayCache then FDirtyB := True;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetAlignment(const Value: TIpHtmlAlign);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if (Value <> haDefault) and (Value <> Alignment) then begin
    CopyPropBRecTo(pRec);
    pRec.Alignment:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetALinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> ALinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.ALinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetBaseFontSize(const Value: Integer);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> BaseFontSize then begin
    CopyPropARecTo(pRec);
    pRec.BaseFontSize:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetBgColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> BgColor then begin
    CopyPropBRecTo(pRec);
    pRec.BgColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontBaseline(const Value: Integer);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontBaseline then begin
    CopyPropBRecTo(pRec);
    pRec.FontBaseline:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontColor then begin
    CopyPropBRecTo(pRec);
    pRec.FontColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontName(const Value: string);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontName then begin
    CopyPropARecTo(pRec);
    pRec.FontName:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontSize(const Value: Integer);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontSize then begin
    CopyPropARecTo(pRec);
    pRec.FontSize:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontStyle(const Value: TFontStyles);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontStyle then begin
    CopyPropARecTo(pRec);
    pRec.FontStyle:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetLinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> LinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.LinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetElemMarginBottom(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginBottom) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginBottom:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginLeft(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginLeft) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginLeft:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginRight(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginRight) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginRight:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginTop(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginTop) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginTop:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetNoBreak(const Value: Boolean);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> NoBreak then begin
    CopyPropBRecTo(pRec);
    pRec.NoBreak:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetPreformatted(const Value: Boolean);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> Preformatted then begin
    CopyPropBRecTo(pRec);
    pRec.Preformatted:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetVAlignment(const Value: TIpHtmlVAlign3);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VAlignment then begin
    CopyPropBRecTo(pRec);
    pRec.VAlignment:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetVLinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VLinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.VLinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetHoverColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverColor then begin
    CopyPropBRecTo(pRec);
    pRec.HoverColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetHoverBgColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverBgColor then begin
    CopyPropBRecTo(pRec);
    pRec.HoverBgColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

{ TIpHtmlPropA }

procedure TIpHtmlPropA.Assign(const Source: TIpHtmlPropA);
begin
  if Source <> nil then begin
    Move(Source.FPropRec, FPropRec, sizeof(TIpHtmlPropAFieldsRec));
  end;
end;

constructor TIpHtmlPropA.CreateCopy(Source: TIpHtmlPropA);
begin
  inherited Create;
  Assign(Source);
end;

procedure TIpHtmlPropA.DecUse;
begin
  if FUseCount > 0 then Dec(FUseCount);
end;

procedure TIpHtmlPropA.IncUse;
begin
  Inc(FUseCount);
end;

procedure TIpHtmlPropA.SetBaseFontSize(const Value: Integer);
begin
  if Value <> FPropRec.BaseFontSize then begin
    FPropRec.BaseFontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontName(const Value: TFontNameStr);
begin
  if Value <> FPropRec.FontName then begin
    FPropRec.FontName := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontSize(const Value: Integer);
begin
  if Value <> FPropRec.FontSize then begin
    FPropRec.FontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontStyle(const Value: TFontStyles);
begin
  if Value <> FPropRec.FontStyle then begin
    FPropRec.FontStyle := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetKnownSizeOfSpace(const Size: TSize);
begin
  FKnownSizeOfSpace := Size;
  FSizeOfSpaceKnown := True;
end;

{ TIpHtmlPropB }

procedure TIpHtmlPropB.Assign(const Source: TIpHtmlPropB);
begin
  if Source <> nil then begin
    Move(Source.FPropRec, FPropRec, sizeof(TIpHtmlPropBFieldsRec));
  end;
end;

constructor TIpHtmlPropB.Create(Owner: TIpHtml);
begin
  inherited Create;
  FPropRec.HoverColor := -1;
  FPropRec.HoverBgColor := -1;
  FOwner := Owner;
end;

constructor TIpHtmlPropB.CreateCopy(Owner: TIpHtml; Source: TIpHtmlPropB);
begin
  inherited Create;
  FOwner := Owner;
  Assign(Source);
end;

procedure TIpHtmlPropB.DecUse;
var
  i: Integer;
begin
  Dec(FUseCount);
  if UseCount = 0 then begin
    for i := Pred(FOwner.PropBCache.Count) downto 0 do
      if TObject(FOwner.PropBCache[i]) = Self then begin
        FOwner.PropBCache.Delete(i);
        Free;
        Exit;
      end;
    raise EIpHtmlException.Create(SHtmlInternal);
  end else
    if UseCount < 0 then
      raise EIpHtmlException.Create(SHtmlInternal);
end;

procedure TIpHtmlPropB.IncUse;
begin
  Inc(FUseCount);
end;

{ TIpHtmlNodeTableHeaderOrCell }

procedure TIpHtmlNodeTableHeaderOrCell.CalcMinMaxPropWidth(const RenderProps: TIpHtmlProps;
  var Min, Max: Integer);
var
  TmpBGColor, TmpFontColor: TColor;
begin
  TmpBGColor := Props.BgColor;
  TmpFontColor := Props.FontColor;
  Props.Assign(RenderProps);
  Props.BgColor := TmpBGColor;
  Props.FontColor := TmpFontColor;
  Props.Alignment := Align;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := VAlign;
  if NoWrap then
    Props.NoBreak := True;
  inherited CalcMinMaxPropWidth(Props, Min, Max);
  if NoWrap then
    Min := Max;
end;

procedure TIpHtmlNodeTableHeaderOrCell.Render(const RenderProps: TIpHtmlProps);
var
  R : TRect;
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  {$IFDEF IP_LAZARUS}
  LoadAndApplyCSSProps;
  {$ENDIF}
//DebugLn('td :', IntToStr(Integer(Props.Alignment)));
  if BgColor <> -1 then
    Props.BgColor := BgColor;
  if Align <> haDefault then
    Props.Alignment := Align
  else if Props.Alignment = haDefault then
  begin
    if Self is TIpHtmlNodeTH then
      Props.Alignment := haCenter
    else
      Props.Alignment := haLeft;
  end;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := VAlign;
  if NoWrap then
    Props.NoBreak := True;
  {$IFDEF IP_LAZARUS_DBG}
  DebugBox(Owner.Target, PadRect, clYellow, True);
  {$ENDIF}
  if PageRectToScreen(PadRect, R) then
  begin
    if (Props.BgColor <> -1) then
    begin
      Owner.Target.Brush.Color := Props.BGColor;
      Owner.Target.FillRect(R);
    end;
  end;
  Props.DelayCache:=False;
  inherited Render(Props);
end;

constructor TIpHtmlNodeTableHeaderOrCell.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FRowSpan := 1;
  FColSpan := 1;
  FAlign := haDefault;
  FVAlign := hva3Middle;
  {FHeight := -1;}
  BgColor := -1;
end;

procedure TIpHtmlNodeTableHeaderOrCell.Layout(
  const RenderProps: TIpHtmlProps; const TargetRect: TRect);
begin
  Props.Assign(RenderProps);
  if Align <> haDefault then
    Props.Alignment := Align
  else
    if Self is TIpHtmlNodeTH then
      Props.Alignment := haCenter;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  if NoWrap then
    Props.NoBreak := True;
  case VAlign of
  hva3Default :;
  else
    Props.VAlignment := VAlign;
  end;
  if BgColor <> -1 then
    Props.BgColor := BgColor;
  inherited Layout(Props, TargetRect);
end;

destructor TIpHtmlNodeTableHeaderOrCell.Destroy;
begin
  inherited;
  FWidth.Free;
  FHeight.Free;
end;

procedure TIpHtmlNodeTableHeaderOrCell.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeInline }

procedure TIpHtmlNodeInline.Invalidate;
begin
  FParentNode.Invalidate;
end;

procedure TIpHtmlNodeInline.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FParentNode.EnqueueElement(Entry);
end;

function TIpHtmlNodeInline.ElementQueueIsEmpty: Boolean;
begin
  Result := FParentNode.ElementQueueIsEmpty;
end;

{ TIpHtmlNodeAlignInline }

constructor TIpHtmlNodeAlignInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Element := Owner.NewElement(etObject, Self);
  Element.Props := Props;
end;

destructor TIpHtmlNodeAlignInline.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeAlignInline.Enqueue;
begin
  EnqueueElement(Element);
end;

procedure TIpHtmlNodeAlignInline.SetAlignment(
  const Value: TIpHtmlImageAlign);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TIpHtmlNodeAlignInline.SetRect(TargetRect: TRect);
begin
end;

{ TIpHtmlNodeControl }

procedure TIpHtmlNodeControl.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if FControl <> nil then
    Min := FControl.Width
  else
    Min := 0;
  Max := Min;
end;

constructor TIpHtmlNodeControl.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.FControlList.Add(Self);
  Align := hiaBottom;
end;

destructor TIpHtmlNodeControl.Destroy;
begin
  Owner.FControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeControl.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
begin
  if FControl <> nil then begin
    TopLeft := Element.WordRect2.TopLeft;
    R.TopLeft := TopLeft;
    Dim := GetDim(0);
    R.Right := TopLeft.x + Dim.cx;
    R.Bottom := TopLeft.y + Dim.cy;
    if PageRectToScreen(R, R) then begin
      FControl.Left := R.Left;
      FCOntrol.Top := R.Top;
      FControl.Visible := True;
      Shown := not ScaleBitmaps{True}; {Keep controls hidden during printing}
    end else
      FControl.Visible := False;
  end;
end;

function TIpHtmlNodeControl.adjustFromCss:boolean;
begin
   result := false;
   {$IFDEF IP_LAZARUS}
   LoadAndApplyCSSProps;
   if (props.FontSize <> -1) then
     FControl.Font.Size:= Props.FontSize;
     if Props.FontColor <> -1 then
       FControl.Font.Color:= Props.FontColor;
     if Props.BGColor <> -1 then
       FControl.Brush.Color:= Props.BGColor;
   result := True;
   {$ENDIF}
end;

procedure TIpHtmlNodeControl.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  {$IFDEF IP_LAZARUS}
  LoadAndApplyCSSProps;
  {$ENDIF}
end;

function TIpHtmlNodeControl.GetDim(ParentWidth: Integer): TSize;
begin
  if FControl <> nil then
    Result := SizeRec(FControl.Width, FControl.Height)
  else
    Result := SizeRec(0, 0);
end;

procedure TIpHtmlNodeControl.HideUnmarkedControl;
begin
  if not Shown and (FControl <> nil) then
    FControl.Visible := False;
end;

procedure TIpHtmlNodeControl.UnmarkControl;
begin
  Shown := False;
end;

{ TIpHtmlNodeNv }

procedure TIpHtmlNodeNv.Invalidate;
begin
end;

procedure TIpHtmlNodeNv.InvalidateSize;
begin
end;

procedure TIpHtmlNodeNv.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

procedure TIpHtmlNodeNv.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNodeNv.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNodeNv.Enqueue;
begin
end;

function TIpHtmlNodeNv.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

{ TIpHtmlNodeFRAME }

procedure TIpHtmlNodeFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetScrolling(
  const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeFRAMESET }

destructor TIpHtmlNodeFRAMESET.Destroy;
begin
  inherited;
  FCols.Free;
  FRows.Free;
end;

{ TIpHtmlNodeGenInline }

constructor TIpHtmlNodeGenInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeGenInline.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeGenInline.SetProps(const RenderProps: TIpHtmlProps);
begin
  ApplyProps(RenderProps);
  inherited SetProps(Props);
end;

{ TIpHtmlInternalPanel }

constructor TIpHtmlInternalPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
  DragMode := dmManual;
  HScroll := TIpHtmlScrollBar.Create(Self, sbHorizontal);
  HScroll.Tracking := True;
  VScroll := TIpHtmlScrollBar.Create(Self, sbVertical);
  VScroll.Tracking := True;
  HintWindow := THintWindow.Create(Self);
  HintWindow.Color := Application.HintColor;
end;

destructor TIpHtmlInternalPanel.Destroy;
begin
  HScroll.Free;
  VScroll.Free;
  HintWindow.Free;
  inherited;
end;

procedure TIpHtmlInternalPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_HSCROLL or WS_VSCROLL;
end;

procedure TIpHtmlInternalPanel.DoHotChange;
begin
  if assigned(FOnHotChange) then
    FOnHotChange(Self);
end;

procedure TIpHtmlInternalPanel.DoCurElementChange;
begin
  if assigned(FOnCurElementChange) then
    FOnCurElementChange(Self);
end;

procedure TIpHtmlInternalPanel.DoHotInvoke;
begin
  if assigned(FOnHotClick) then
    FOnHotClick(Hyper);
end;

procedure TIpHtmlInternalPanel.DoClick;
begin
  if assigned(FOnClick) then
    FOnClick(Hyper);
end;

procedure TIpHtmlInternalPanel.ShowHintNow(const NewHint: string);
var
  Tw,Th : Integer;
  Sc : TPoint;
  {$IFNDEF IP_LAZARUS}
  IPHC: TIpHtmlCustomPanel;
  {$ENDIF}
begin
  {$IFDEF IP_LAZARUS}
  if HtmlPanel.ShowHints then begin
  {$ELSE}
  IPHC := HtmlPanel;
  if Assigned (IPHC) and IPHC.ShowHints and (NewHint <> CurHint) then begin
  {$ENDIF}
    {$IFDEF IP_LAZARUS}
    if (NewHint<>'') then begin
      Tw := HintWindow.Canvas.TextWidth(NewHint);
      Th := HintWindow.Canvas.TextHeight(NewHint);
      Sc := ClientToScreen(Point(HintX,HintY));
      HintWindow.ActivateHint(Rect(Sc.X + 6,
                                   Sc.Y + 16 - 6,
                                   Sc.X + Tw + 18,
                                   Sc.Y + Th + 16 + 6),
                              NewHint);
    end else
      HideHint;
    {$ELSE}
    if (NewHint <> '') and not IsWindowVisible(HintWindow.Handle) then begin
      Tw := HintWindow.Canvas.TextWidth(NewHint);
      Th := HintWindow.Canvas.TextHeight(NewHint);
      Sc := ClientToScreen(Point(HintX,HintY));
      HintWindow.ActivateHint(Rect(Sc.X + 4,
                                   Sc.Y + 16,
                                   Sc.X + Tw + 12,
                                   Sc.Y + Th + 16),
                              NewHint);
    end else
      HideHint;
    {$ENDIF}
    CurHint := NewHint;
    HintShownHere := True;
  end;
end;

procedure TIpHtmlInternalPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHot : TIpHtmlNode;
  OldCurElement : PIpHtmlElement;
  {$IFNDEF IP_LAZARUS}
  IPHC: TIpHtmlCustomPanel;
  {$ENDIF}
  TmpOwnerNode: TIpHtmlNode;
begin
  if MouseIsDown and HaveSelection then begin
    SelEnd := Point(X + ViewLeft, Y + ViewTop);
    SetSelection;
    ScrollPtInView(Point(X + ViewLeft, Y + ViewTop));
  end;
  if Hyper <> nil then begin
    OldHot := Hyper.HotNode;
    OldCurElement := Hyper.CurElement;
    Hyper.MouseMove(Point(X + ViewLeft, Y + ViewTop));
    if (Hyper.HotNode <> OldHot) or (Hyper.HotPoint.x >= 0) then
      DoHotChange;
    if Hyper.HotNode <> nil then begin
      if Hyper.CurElement <> nil then begin
        Hyper.CurElement := nil;
        if OldCurElement <> Hyper.CurElement then
          DoCurElementChange;
      end;
    end else begin
      {$IFDEF IP_LAZARUS}
      if HtmlPanel.AllowTextSelect then begin
      {$ELSE}
      IPHC := HtmlPanel;
      if Assigned (IPHC) and IPHC.AllowTextSelect then begin
      {$ENDIF}
        if Hyper.CurElement <> nil then begin
          if Hyper.CurElement.ElementType = etWord then
            Cursor := crIBeam
          else
            Cursor := crDefault;
        end else
          Cursor := crDefault;
      end;
      if OldCurElement <> Hyper.CurElement then
        DoCurElementChange;
    end;
  end;
  if (Hyper <> nil) and (Hyper.HotNode <> nil) then
    Hint := Hyper.HotNode.GetHint
  else
  if (Hyper <> nil) and (Hyper.CurElement <> nil)
  and (Hyper.CurElement.ElementType = etObject)
  and (Hyper.CurElement.Owner <> nil) then
    Hint := Hyper.CurElement.Owner.GetHint
  else
    Hint := '';
  {$IFNDEF IP_LAZARUS}
  if NewSelection then begin
    ClearSelection;
    SelStart := Point(X + ViewLeft, Y + ViewTop);
    NewSelection := False;
    HaveSelection := True;
  end;
  {$ENDIF}
  inherited;

  // show hints for IpHtmlTagABBR and IpHtmlTagACRONYM
  if (Hyper <> nil) and (Hyper.CurElement <> nil) then begin

    TmpOwnerNode := Hyper.CurElement.Owner;
    while TmpOwnerNode <> nil do begin
      if TmpOwnerNode is TIpHtmlNodePhrase then begin
        if (TIpHtmlNodePhrase(TmpOwnerNode).Style = hpsABBR) or (TIpHtmlNodePhrase(TmpOwnerNode).Style = hpsACRONYM) then begin
          Hint := TIpHtmlNodePhrase(TmpOwnerNode).Title;
          Break;
        end else begin
          TmpOwnerNode := TmpOwnerNode.FParentNode;
        end;
      end else begin
        TmpOwnerNode := TmpOwnerNode.FParentNode;
      end;
    end;

  end;

  // "refresh" hint if it should have new value OR cursors position changes significantly (then we reposition the hint with the same text)
  if (Hint <> CurHint) or ((abs(HintX - X) > 4) or (abs(HintY - Y) > 4)) then begin
    HintShownHere := False;
    HintX := X;
    HintY := Y;
  end;
  if not HintShownHere then
    ShowHintNow(Hint);
end;

procedure TIpHtmlInternalPanel.HideHint;
begin
  {$IFDEF IP_LAZARUS}
  HintWindow.Visible := False;
  {$ELSE}
  HintWindow.ReleaseHandle;
  {$ENDIF}
end;

procedure TIpHtmlInternalPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{$IFNDEF IP_LAZARUS}
var
  IPHC: TIpHtmlCustomPanel;
{$ENDIF}
begin
  MouseDownX := X;
  MouseDownY := Y;
  MouseIsDown := True;
  {$IFDEF IP_LAZARUS}
  Self.SetFocus;
  if (Button=mbLeft) and HtmlPanel.AllowTextSelect then begin
    ClearSelection;
    SelStart := Point(X + ViewLeft, Y + ViewTop);
    NewSelection := False;
    HaveSelection := True;
  end;
  {$ELSE}
  IPHC := HtmlPanel;
  if  Assigned (IPHC)
  then  NewSelection := IPHC.AllowTextSelect and (Button = mbLeft);
  {$ENDIF}
  inherited;
end;

procedure TIpHtmlInternalPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  MouseIsDown := False;
  if (abs(MouseDownX - X) < 4)
  and (abs(MouseDownY - Y) < 4) then
    if (Button = mbLeft) and (Hyper.HotNode <> nil) then
      {$IFDEF IP_LAZARUS}
      // to avoid references to invalid objects do it asynchronously
      Application.QueueAsyncCall(AsyncHotInvoke, 0)
      {$ELSE}
      DoHotInvoke
      {$ENDIF}
    else
      DoClick;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlInternalPanel.MouseLeave;
begin
  HideHint;
  inherited MouseLeave;
end;

procedure TIpHtmlInternalPanel.KeyDown(var Key: Word; Shift: TShiftState);
var
  TabList: TIpHtmlTabList;
begin
  if (key = VK_TAB) and TIpHtmlCustomPanel(Owner).WantTabs then
  begin
    TabList := FHyper.FTabList;

    if TabList.Index = -1 then
    begin
      // TODO find best place to start the index at...
      TabList.Index := 0;
    end;

    if (TabList.Count > 0) then
    begin
      if TIpHtmlNode(TabList[TabList.Index]) is TIpHtmlNodeA then
        TIpHtmlNodeA(TabList[TabList.Index]).DoOnBlur
      else if TObject(TabList[TabList.Index]).InheritsFrom(TIpHtmlNodeControl) then
        TIpHtmlNodeControl(TabList[TabList.Index]).FControl.Parent.SetFocus;

      if (ssShift in Shift) then
      begin
        if (TabList.Index > 0) then
        begin
          TabList.Index := TabList.Index -1;
          Key := 0;
        end
        else
          TabList.Index:=TabList.Count-1;
      end;

      if not(ssShift in Shift) then
      begin
        if TabList.Index < TabList.Count-1 then
        begin
          TabList.Index := TabList.Index + 1;
          Key := 0;
        end
        else
          TabList.Index := 0;
      end;

      if Key = 0 then
      begin
        if TIpHtmlNode(TabList[TabList.Index]) is TIpHtmlNodeA then
          TIpHtmlNodeA(TabList[TabList.Index]).DoOnFocus
        else if TObject(TabList[TabList.Index]).InheritsFrom(TIpHtmlNodeControl) then
          TIpHtmlNodeControl(TabList[TabList.Index]).FControl.SetFocus;
      end;
    end;
  end
  else if (key = VK_PRIOR) or ((key = VK_SPACE) and (ssShift in Shift)) then // page up
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaPgUp);
    Key := 0
  end
  else if (key = VK_NEXT) or ((key = VK_SPACE) and not(ssShift in Shift)) then // page down
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaPgDn);
    Key := 0
  end
  else if key = VK_UP then // up
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaUp);
    Key := 0
  end
  else if key = VK_DOWN then // down
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaDown);
    Key := 0
  end
  else if key = VK_LEFT then // left
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaLeft);
    Key := 0
  end
  else if key = VK_RIGHT then // right
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaRight);
    Key := 0
  end
  else if key = VK_HOME then // home
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaHome);
    Key := 0
  end
  else if key = VK_END then // end
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaEnd);
    Key := 0
  end
  else if key = VK_RETURN then // return
  begin
    if (FHyper.FTabList.TabItem <> nil) and (FHyper.FTabList.TabItem is TIpHtmlNodeA) then
    begin
      TIpHtmlNodeA(FHyper.FTabList.TabItem).Hot:=True;
      FHyper.FHotNode := TIpHtmlNodeA(FHyper.FTabList.TabItem);

      DoHotChange;
      Application.QueueAsyncCall(AsyncHotInvoke, 0);
      Key := 0
    end;
  end
  else
    inherited KeyDown(Key, Shift);
end;

{$ENDIF}

function TIpHtmlInternalPanel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  i: Integer;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  for i := 0 to Mouse.WheelScrollLines-1 do
    if WheelDelta < 0 then
      Perform({$IFDEF IP_LAZARUS}LM_VSCROLL{$ELSE}WM_VSCROLL{$ENDIF}, MAKELONG(SB_LINEDOWN, 0), 0)
    else
      Perform({$IFDEF IP_LAZARUS}LM_VSCROLL{$ELSE}WM_VSCROLL{$ENDIF}, MAKELONG(SB_LINEUP, 0), 0);
end;

procedure TIpHtmlInternalPanel.Paint;
var
  CR : TRect;
begin
  CR := GetClientRect;
  if not ScaleBitmaps {printing}
  and (Hyper <> nil) then begin
    // update layout
    GetPageRect;
    // render
    Hyper.Render(Canvas,
      Rect(
        ViewLeft, ViewTop,
          ViewLeft + (CR.Right - CR.Left),
          ViewTop + (CR.Bottom - CR.Top)),
          True,
          Point(0, 0))
  end
  else
    Canvas.FillRect(CR);
  //debugln(['TIpHtmlInternalPanel.Paint ',dbgs(CR)]);
  {$IFDEF IP_LAZARUS_DBG}
  DebugBox(Canvas, CR, clYellow);
  Debugbox(Canvas, Canvas.ClipRect, clLime, true);
  {$ENDIF}
end;

procedure TIpHtmlInternalPanel.BeginPrint;
begin
  if InPrint = 0 then begin
    Printed := False;
    ScaleBitmaps := True;
    ResetPrint;
  end;
  Inc(InPrint);
end;

procedure TIpHtmlInternalPanel.EndPrint;
begin
  Dec(InPrint);
  if InPrint = 0 then begin
    ScaleBitmaps := False;
    InvalidateSize;
  end;
end;

procedure TIpHtmlInternalPanel.ResetPrint;
var
  LogPixX, LMarginPix, RMarginPix,
  LogPixY, TMarginPix, BMarginPix,
  H: Integer;
begin
  // check ir BeginPrint was called
  if not Printed then begin
    SetRectEmpty(PrintPageRect);
    if Hyper.TitleNode <> nil then
      Printer.Title := Hyper.TitleNode.Title
    else
      Printer.Title := 'HTML Document';
    Printer.BeginDoc;
    GetRelativeAspect(Printer.Canvas.Handle);
    {$IF DEFINED(IP_LAZARUS) AND NOT DEFINED(WINDOWS)}
    // this test looks weird, according to most references consulted, the number
    // of colors in a display is NColors = 1 shl (bitsPerPixel * Planes). A mono
    // printer should have 2 colors, somebody else needs to clarify.
    BWPrinter := false;
    {$ELSE}
    BWPrinter := GetDeviceCaps(Printer.Canvas.Handle, PLANES) = 1;
    {$ENDIF}
    {$IFDEF IP_LAZARUS}
    LogPixX := Printer.XDPI;
    {$ELSE}
    LogPixX := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
    {$ENDIF}
    LMarginPix := round(HtmlPanel.PrintSettings.MarginLeft * LogPixX);
    RMarginPix := round(HtmlPanel.PrintSettings.MarginRight * LogPixX);
    PrintWidth := Printer.PageWidth - LMarginPix - RMarginPix;
    {$IFDEF IP_LAZARUS}
    LogPixY := Printer.YDPI;
    {$ELSE}
    LogPixY := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY);
    {$ENDIF}
    TMarginPix := round(HtmlPanel.PrintSettings.MarginTop * LogPixY);
    BMarginPix := round(HtmlPanel.PrintSettings.MarginBottom * LogPixY);
    if Printer.Printers.Count = 0 then begin
      PrintHeight := 500;
    end else begin
      PrintHeight := Printer.PageHeight - TMarginPix - BMarginPix;
    end;
    PrintTopLeft := Point(LMarginPix, TMarginPix);
    {PrintBottomRight := Point(
      Printer.PageWidth - RMarginPix,
      Printer.PageHeight - BMarginPix);}
    PrintPageRect := Hyper.GetPageRect(Printer.Canvas, PrintWidth, PrintHeight);
    H := PrintPageRect.Bottom - PrintPageRect.Top;
    PageCount := H div PrintHeight;
    if H mod PrintHeight <> 0 then
      Inc(PageCount);
    Printer.Abort;
  end else
    raise Exception.Create('BeginPrint must be called before ResetPrint.');
end;

function TIpHtmlInternalPanel.SelectPrinterDlg: boolean;
var
  printDialog: TPrintDialog;
begin
  Result := False;
  printDialog := TPrintDialog.Create(nil);
  if printDialog.Execute then begin
    ResetPrint;
    Result := true;
  end;
end;

procedure TIpHtmlInternalPanel.PrintPages(FromPage, ToPage: Integer);
var
  CR : TRect;
var
  i : Integer;
begin
  {CR := Rect(0, 0, Printer.PageWidth, 0);}
  if (Hyper <> nil) then begin
    BeginPrint;
    Printer.BeginDoc;
    try
      (*
      ScaleBitmaps := True;
      GetRelativeAspect(Printer.Canvas.Handle);
      PrintPageRect := Hyper.GetPageRect(Printer.Canvas,
        Printer.PageWidth, Printer.PageHeight);
      *)
      CR := Rect(0, 0, PrintWidth, 0);
      for i := FromPage to ToPage do begin
        CR.Top := (i - 1) * PrintHeight;
        CR.Bottom := Cr.Top + PrintHeight;
        Hyper.Render(Printer.Canvas, CR, False, PrintTopLeft);
        if i < ToPage then
          Printer.NewPage;
        Printed := True;
      end;
    finally
      {ScaleBitmaps := False;}
      if Printed then
        Printer.EndDoc
      else
        Printer.Abort;
      {InvalidateSize;}
      EndPrint;
    end;
  end;
end;

procedure TIpHtmlInternalPanel.PrintPreview;
begin
  if (Hyper <> nil) then begin
    BeginPrint;
    try

      with TIpHTMLPreview.Create(Application) do
        try
          lblMaxPage.Caption := IntToStr(PageCount);
          FCurPage := 1;
          HTML := Hyper;
          ScaleFonts := True;
          try
            OwnerPanel := Self;
            ShowModal;
          finally
            ScaleFonts := False;
          end;
        finally
          Free;
        end;

    finally
      EndPrint;
    end;
  end;
end;

procedure TIpHtmlInternalPanel.EraseBackground(DC: HDC);
begin
  //
end;

function TIpHtmlInternalPanel.GetPrintPageCount: Integer;
begin
  BeginPrint;
  try
    Result := PageCount;
  finally
    EndPrint;
  end;
end;

procedure TIpHtmlInternalPanel.InvalidateSize;
begin
  FPageRectValid:=false;
  Invalidate;
end;

procedure TIpHtmlInternalPanel.Resize;
begin
  inherited;
  InvalidateSize;
end;

function TIpHtmlInternalPanel.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  Dec(Result.x, ViewLeft);
  Dec(Result.y, ViewTop);
end;

procedure TIpHtmlInternalPanel.ScrollInViewRaw(R : TRect);
begin
  R.TopLeft := PagePtToScreen(R.TopLeft);
  R.BottomRight := PagePtToScreen(R.BottomRight);
  if R.Left < 0 then
    with HScroll do
     Position := Position + R.Left
  else if R.Right > ClientWidth then begin
    if R.Right - R.Left > ClientWidth then
      R.Right := R.Left + ClientWidth;
    with HScroll do
      Position := Position + R.Right - ClientWidth;
  end;
  if R.Top < 0 then
    with VScroll do
      Position := Position + R.Top
  else if R.Bottom > ClientHeight then begin
    if R.Bottom - R.Top > ClientHeight then
      R.Bottom := R.Top + ClientHeight;
    with VScroll do
      Position := Position + R.Bottom - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollInView(R : TRect);
begin
  R.Bottom := R.Top + (ClientHeight - (R.Bottom - R.Top) - 10);
  R.Right := R.Left + (ClientWidth - (R.Right - R.Left) - 10);
  ScrollInViewRaw(R);
end;

procedure TIpHtmlInternalPanel.ScrollPtInView(P : TPoint);
begin
  P := PagePtToScreen(P);
  if P.x < 0 then
    with HScroll do
     Position := Position + P.x
  else if P.x > ClientWidth then begin
    with HScroll do
      Position := Position + P.x - ClientWidth;
  end;
  if P.y < 0 then
    with VScroll do
      Position := Position + P.y
  else if P.y > ClientHeight then begin
    with VScroll do
      Position := Position + P.y - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollRequest(Sender: TIpHtml; const R: TRect{$IFDEF IP_LAZARUS}; ShowAtTop: Boolean = True{$ENDIF});
begin
  {$IFDEF IP_LAZARUS}
  if not ShowAtTop then
    ScrollInViewRaw(R)
  else
  {$ENDIF}
  ScrollInView(R);
end;

procedure TIpHtmlInternalPanel.SetHtml(const Value: TIpHtml);
begin
  FHyper := Value;
  InvalidateSize;
end;

function TIpHtmlInternalPanel.GetPageRect: TRect;
begin
  if not FPageRectValid then begin
    if Hyper <> nil then
      PageRect := Hyper.GetPageRect(Canvas, ClientWidth, 0)
    else
      PageRect:=Rect(0,0,0,0);
    FPageRectValid:=true;
  end;
  Result:=FPageRect;
end;

procedure TIpHtmlInternalPanel.SetPageRect(const Value: TRect);
begin
  if not SettingPageRect then begin
    SettingPageRect := True;
    FPageRect := Value;
    HScroll.CalcAutoRange;
    VScroll.CalcAutoRange;
    SettingPageRect := False;
  end;
end;

procedure TIpHtmlInternalPanel.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then
    try
      FUpdatingScrollBars := True;
      if VScroll.NeedsScrollBarVisible then
      begin
        HScroll.Update(False, True);
        VScroll.Update(True, False);
      end
      else if HScroll.NeedsScrollBarVisible then
      begin
        VScroll.Update(False, True);
        HScroll.Update(True, False);
      end
      else
      begin
        VScroll.Update(False, False);
        HScroll.Update(True, False);
      end;
      GetPageRect();
    finally
      FUpdatingScrollBars := False;
    end;
end;

procedure TIpHtmlInternalPanel.WMHScroll(var Message: {$IFDEF IP_LAZARUS}TLMHScroll{$ELSE}TWMHScroll{$ENDIF});
begin
  {$IFDEF IP_LAZARUS}
  if HScroll.Visible then
    HScroll.ScrollMessage(Message);
  {$ELSE}
  if (Message.ScrollBar = 0) and HScroll.Visible then
    HScroll.ScrollMessage(Message) else
    inherited;
  {$ENDIF}
end;

procedure TIpHtmlInternalPanel.WMVScroll(var Message: {$IFDEF IP_LAZARUS}TLMVScroll{$ELSE}TWMVScroll{$ENDIF});
begin
  {$IFDEF IP_LAZARUS}
  if VScroll.Visible then
    VScroll.ScrollMessage(Message);
  {$ELSE}
  if (Message.ScrollBar = 0) and VScroll.Visible then
    VScroll.ScrollMessage(Message) else
    inherited;
  {$ENDIF}
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlInternalPanel.AsyncHotInvoke(data: ptrint);
begin
  DoHotInvoke;
end;
{$ENDIF}

procedure TIpHtmlInternalPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TIpHtmlInternalPanel.ClearSelection;
begin
  Hyper.SetSelection(Point(-1, -1), Point(-1, -1));
  HaveSelection := False;
end;

procedure TIpHtmlInternalPanel.SetSelection;
begin
  if Hyper <> nil then
    Hyper.SetSelection(SelStart, SelEnd);
end;

function TIpHtmlInternalPanel.HtmlPanel: TIpHtmlCustomPanel;
begin
  Result := TIpHtmlPanel(Parent);
  {$IFDEF IP_LAZARUS}
  while not (Result is TIpHtmlPanel) do
  {$ELSE}
  while  Assigned(Result) and (Result.ClassType <> TIpHtmlPanel)  do
  {$ENDIF}
    Result := TIpHtmlPanel(Result.Parent);
end;

{ TIpHtmlScrollBar }

constructor TIpHtmlScrollBar.Create(AControl: TIpHtmlInternalPanel;
  AKind: TScrollBarKind);
begin
  inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPageIncrement := 80;
  FIncrement := FPageIncrement div 10;
  FVisible := True;
  FUpdateNeeded := True;
end;

procedure TIpHtmlScrollBar.CalcAutoRange;
begin
  if Kind = sbHorizontal then
    DoSetRange(FControl.PageRect.Right)
  else
    DoSetRange(FControl.PageRect.Bottom);
end;

function TIpHtmlScrollBar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not ControlSB then
      if AssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else if not AssumeSB and ScrollBarVisible(Code) then
        Result := GetSystemMetrics(Metric) - BorderAdjust;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(FControl.Handle, GWL_STYLE) and
    (WS_BORDER or WS_THICKFRAME) <> 0);
  if Kind = sbVertical then
    Result := FControl.ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL) else
    Result := FControl.ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

function TIpHtmlScrollBar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False);
end;

procedure TIpHtmlScrollBar.ScrollMessage(var Msg: {$IFDEF IP_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});

  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then
      Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(FControl.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
    case ScrollCode of
    SB_LINEUP:
      SetPosition(FPosition - FIncrement);
    SB_LINEDOWN:
      SetPosition(FPosition + FIncrement);
    SB_PAGEUP:
      SetPosition(FPosition - ControlSize(True, False));
    SB_PAGEDOWN:
      SetPosition(FPosition + ControlSize(True, False));
    SB_THUMBPOSITION:
      if FCalcRange > 32767 then
        SetPosition(GetRealScrollPosition)
      else
        SetPosition(Pos);
    SB_THUMBTRACK:
      if Tracking then
        if FCalcRange > 32767 then
          SetPosition(GetRealScrollPosition)
        else
          SetPosition(Pos);
    SB_TOP:
      SetPosition(0);
    SB_BOTTOM:
      SetPosition(FCalcRange);
    SB_ENDSCROLL:
      ;
    end;
end;

procedure TIpHtmlScrollBar.SetPosition(Value: Integer);
var
  Code: Word;
begin
  if csReading in FControl.ComponentState then
    FPosition := Value
  else begin
    if Value > FCalcRange then Value := FCalcRange
    else if Value < 0 then Value := 0;
    if Kind = sbHorizontal then
      Code := SB_HORZ else
      Code := SB_VERT;
    if Value <> FPosition then
    begin
      FPosition := Value;
      if Kind = sbHorizontal then
        FControl.ViewLeft := Value
      else
        FControl.ViewTop := Value;
      FControl.Invalidate;
    end;
    if FlatSB_GetScrollPos(FControl.Handle, Code) <> FPosition then
      FlatSB_SetScrollPos(FControl.Handle, Code, FPosition, True);
  end;
end;

procedure TIpHtmlScrollBar.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.Update(ControlSB, AssumeSB: Boolean);
type
  TPropKind = (pkStyle, pkButtonSize, pkThumbSize, pkSize, pkBkColor);
const
  Props: array[TScrollBarKind, TPropKind] of Integer = (
    (WSB_PROP_HSTYLE, WSB_PROP_CXHSCROLL, WSB_PROP_CXHTHUMB, WSB_PROP_CYHSCROLL,
     WSB_PROP_HBKGCOLOR),
    (WSB_PROP_VSTYLE, WSB_PROP_CYVSCROLL, WSB_PROP_CYVTHUMB, WSB_PROP_CXVSCROLL,
     WSB_PROP_VBKGCOLOR));
var
  Code: Word;
  ScrollInfo: TScrollInfo;
  iPi: integer;

  procedure UpdateScrollProperties(Redraw: Boolean);
  begin
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkStyle], FSB_REGULAR_MODE, Redraw);
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkBkColor],
      integer(ColorToRGB(clBtnHighlight)), False);
  end;

begin
  FCalcRange := 0;
  Code := SB_HORZ;
  if Kind = sbVertical then
    Code := SB_VERT;
  if Visible then begin
    FCalcRange := Range - ControlSize(ControlSB, AssumeSB);
    if FCalcRange < 0 then
      FCalcRange := 0;
  end;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  if FCalcRange > 0 then
    ScrollInfo.nMax := Range
  else
    ScrollInfo.nMax := 0;
  iPi := ControlSize(ControlSB, AssumeSB) + 1;
  if iPi < 1 then iPi := 1;
  ScrollInfo.nPage := iPi;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;
  UpdateScrollProperties(FUpdateNeeded);
  FUpdateNeeded := False;
  FlatSB_SetScrollInfo(FControl.Handle, Code, ScrollInfo, True);
  SetPosition(FPosition);
  iPi := (ControlSize(True, False) * 9) div 10;
  if iPi < low(TScrollbarInc) then iPi := low(TScrollbarInc)
  else if iPi > high(TScrollbarInc) then iPi := high(TScrollbarInc);
  FPageIncrement := iPi;
end;


{$IFNDEF IP_LAZARUS}
{ TIpHtmlFocusRect }

constructor TIpHtmlFocusRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csReplicatable, csDoubleClicks];
  Width := 65;
  Height := 17;
end;

procedure TIpHtmlFocusRect.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do begin
    {$IFNDEF IP_LAZARUS}
    Style := Style or SS_NOTIFY;
    {$ENDIF}
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlFocusRect.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  Anchor.DoOnFocus;
end;

procedure TIpHtmlFocusRect.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  Anchor.DoOnBlur;
  {HaveFocus := False;}
end;

{$ELSE}
procedure TIpHtmlFocusRect.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Anchor.DoOnFocus;
end;

procedure TIpHtmlFocusRect.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Anchor.DoOnBlur;
  {HaveFocus := False;}
end;

{$ENDIF}
{$ENDIF}
{ TIpHtmlFrame }

procedure TIpHtmlFrame.InitHtml;
begin
  FHtml.FixedTypeface := Viewer.FixedTypeface;
  FHtml.DefaultTypeFace := Viewer.DefaultTypeFace;
  FHtml.DefaultFontSize := Viewer.DefaultFontSize;
  FHtml.TextColor := FViewer.TextColor;
  FHtml.LinkColor := FViewer.LinkColor;
  FHtml.ALinkColor := FViewer.ALinkColor;
  FHtml.VLinkColor := FViewer.VLinkColor;
  if FViewer.DataProvider <> nil then
    FHtml.OnGetImageX := FViewer.DataProvider.DoGetImage;
  FHtml.OnInvalidateRect := InvalidateRect;
  FHtml.OnInvalidateSize := InvalidateSize;
  FHtml.OnGet := Get;
  FHtml.OnPost := Post;
  FHtml.OnIFrameCreate := IFrameCreate;
  FHtml.OnURLCheck := FViewer.URLCheck;
  FHtml.OnReportURL := FViewer.ReportURL;
  FHtml.FlagErrors := FFlagErrors;
  FHtml.MarginWidth := FMarginWidth;
  FHtml.MarginHeight := FMarginHeight;
  {$IFDEF IP_LAZARUS}
  if FDataProvider <> nil then
    FHtml.FDataProvider := FDataProvider;
  {$ENDIF}
  FHtml.FactBAParag := FViewer.FactBAParag;
end;

constructor TIpHtmlFrame.Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
begin
  FNoScroll := NoScroll;
  FParent := Parent;
  FViewer := Viewer;
  FDataProvider := DataProvider;
  FHtml := TIpHtml.Create;
  FFlagErrors := FlagErrors;
  FMarginWidth := MarginWidth;
  FMarginheight := MarginHeight;
  InitHtml;
end;

destructor TIpHtmlFrame.Destroy;
var
  i : Integer;
begin
  if FFramePanel <> nil then
    FFramePanel.OnResize := nil;
  for i := 0 to Pred(FFrameCount) do
    FreeAndNil(FFrames[i]);
  if HyperPanel <> nil then begin
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  //debugln(['TIpHtmlFrame.Destroy ',DbgSName(Self),' ',dbgs(Pointer(FDataProvider))]);
  if (FDataProvider <> nil) and (not (csDestroying in FDataProvider.ComponentState)) then
    FDataProvider.DoLeave(FHtml);
  FreeAndNil(FHtml);
  inherited;
end;

procedure TIpHtmlFrame.InvalidateRect(Sender: TIpHtml; const R: TRect);
begin
  if HyperPanel <> nil then
    {$IFDEF IP_LAZARUS}
    LCLIntf.InvalidateRect(HyperPanel.Handle, @R, False);
    {$ELSE}
    Windows.InvalidateRect(HyperPanel.Handle, @R, False);
    {$ENDIF}
end;

procedure TIpHtmlFrame.InvalidateSize(Sender: TObject);
begin
  if HyperPanel <> nil then
    if not InOpen then
      HyperPanel.InvalidateSize;
end;

procedure TIpHtmlFrame.OpenURL(const URL: string; Delayed: Boolean);
begin
  if Delayed then begin
    FViewer.GetURL := URL;
    FViewer.PostURL := '';
    FViewer.PostData := nil;
    PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
  end else
    OpenRelativeURL(URL);
end;

procedure TIpHtmlFrame.AlignPanels;
var
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  N, i, R, C, L, T : Integer;
begin
  if (FHtml = nil) or (FHtml.FrameSet = nil) then Exit;
  if FFramePanel = nil then Exit;
  ColW := CalcMultiLength(FHtml.FrameSet.Cols, FFramePanel.ClientWidth,
    ColWCount);
  try
    RowH := CalcMultiLength(FHtml.FrameSet.Rows, FFramePanel.ClientHeight,
      RowHCount);
    try
      R := 0;
      C := 0;
      L := 0;
      T := 0;
      N := 0;
      for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
        if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
          if Pnl[N] <> nil then
            Pnl[N].SetBounds(L, T, ColW[C], RowH[R]);
          Inc(L, ColW[C]);
          if C < ColWCount - 1 then
            Inc(C)
          else begin
            C := 0;
            L := 0;
            Inc(T, RowH[R]);
            Inc(R);
          end;
          Inc(N);
        end;
      end;
    finally
      RowH.Free;
    end;
  finally
    ColW.Free;
  end;
end;

function TIpHtmlFrame.IsExternal(const URL: string): Boolean;
var
  St, ResourceType : string;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(FCurURL, URL)
  else
    St := IpUtils.BuildURL(FCurURL, URL);
  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);
  St := LowerCase(ResourceType);

  if ( Pos('text/', St) = 0) and (pos('image/', St) = 0) then begin
    FViewer.FHotURL := St;
    FViewer.DoHotClick;
    Result := True;
  end else
    Result := False;
end;

function BuildImagePage(const URL: string): TMemoryStream;
var
  S : string;
begin
  Result := TMemoryStream.Create;
  S := '<Html><BODY><IMG src=';
  Result.Write(S[1], length(S));
  Result.Write(URL[1], length(URL));
  S := '></BODY></Html>';
  Result.Write(S[1], length(S));
  Result.Seek(0, 0);
end;

procedure TIpHtmlFrame.InternalFreeFrames;
var
   i: integer;
begin
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FFramePanel.Free;
  FFramePanel := nil;
  FFrameCount := 0;
  if HyperPanel <> nil then begin
    FHtml.OnScroll := nil;
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
  FHtml.Clear;
  FHtml.Free;
end;

procedure TIpHtmlFrame.InternalCreateFrames;
var
  MW, MH,
  i, R, C, L, T : Integer;
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  Scroll : Boolean;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  ColWCount := 0;
  RowHCount := 0;

  if FHtml.HasFrames then begin
    FFramePanel := TPanel.Create(FParent);
    FFramePanel.BevelOuter := bvNone;
    FFramePanel.Align := alClient;
    FFramePanel.Parent := FParent;
    FFramePanel.OnResize := FramePanelResize;
    FFramePanel.FullRepaint := False;
    ColW := CalcMultiLength(FHtml.FrameSet.Cols, FFramePanel.ClientWidth, ColWCount);
    try
      RowH := CalcMultiLength(FHtml.FrameSet.Rows, FFramePanel.ClientHeight, RowHCount);
      try
        R := 0;
        C := 0;
        L := 0;
        T := 0;
        FFrameCount := 0;
        for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
          if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
            CurFrameDef := TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]);
            Pnl[FFrameCount] := TPanel.Create(FFramePanel);
            Pnl[FFrameCount].BevelOuter := bvNone;
            Pnl[FFrameCount].SetBounds(L, T, ColW[C], RowH[R]);
            Pnl[FFrameCount].Parent := FFramePanel;
            Pnl[FFrameCount].FullRepaint := False;

            if CurFrameDef.FrameBorder <> 0 then begin
              Pnl[FFrameCount].BorderStyle := bsSingle;
              Pnl[FFrameCount].BorderWidth := CurFrameDef.FrameBorder;
            end;

            Inc(L, ColW[C]);

            case CurFrameDef.Scrolling of
            hfsAuto, hfsYes :
              Scroll := True;
            else //hfsNo :
              Scroll := False;
            end;

            if CurFrameDef.MarginWidth <> -1 then
              MW := CurFrameDef.MarginWidth
            else
              MW := FViewer.MarginWidth;
            if CurFrameDef.MarginHeight <> -1 then
              MH:= CurFramedef.MarginHeight
            else
              MH := FViewer.MarginHeight;

            FFrames[FFrameCount] :=
              TIpHtmlFrame.Create(FViewer, Pnl[FFrameCount], FDataProvider,
                FViewer.FlagErrors, not Scroll, MW, MH);
            FFrames[FFrameCount].FName := CurFrameDef.FName;
            if C < ColWCount - 1 then
              Inc(C)
            else begin
              C := 0;
              L := 0;
              Inc(T, RowH[R]);
              Inc(R);
            end;
            Inc(FFrameCount);
          end;
        end;
      finally
        RowH.Free;
      end;
    finally
      ColW.Free;
    end;
    Application.ProcessMessages;
    FFrameCount := 0;
    for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
      if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
        FFrames[FFrameCount].FCurURL := FCurURL;
        FFrames[FFrameCount].OpenRelativeURL({Base,}
          TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]).Src);
        Inc(FFrameCount);
      end;
    end;
  end else begin
    HyperPanel := TIpHtmlInternalPanel.Create(FParent);
    if FNoScroll then begin
      HyperPanel.HScroll.Visible := False;
      HyperPanel.VScroll.Visible := False;
    end;
    HyperPanel.Parent := FParent;
    HyperPanel.Align := alClient;
    HyperPanel.OnHotChange := FViewer.HotChange;
    HyperPanel.OnCurElementChange := FViewer.CurElementChange;
    HyperPanel.OnHotClick := FViewer.HotClick;
    HyperPanel.OnClick := FViewer.ClientClick;
    HyperPanel.TabStop := FViewer.WantTabs;
    FHtml.OnScroll := HyperPanel.ScrollRequest;
    FHtml.OnControlClick := ControlClick;
    FHtml.OnControlClick2 := ControlClick2;
    FHtml.OnControlChange := ControlOnChange;
    FHtml.OnControlEditingdone := ControlOnEditingDone;
    FHtml.OnControlCreate := ControlCreate;
    {$IFNDEF IP_LAZARUS}
    for i := 0 to Pred(FHtml.AnchorList.Count) do
      with TIpHtmlFocusRect.Create(HyperPanel) do begin
        SetBounds(-100, -100, 10, 10);
        TabStop := True;
        Parent := HyperPanel;
        Anchor := FHtml.AnchorList[i];
      end;
    {$ENDIF}
    for i := 0 to Pred(FHtml.FControlList.Count) do
      TIpHtmlNode(FHtml.FControlList[i]).CreateControl(HyperPanel);
    HyperPanel.Hyper := FHtml;
  end;
end;

procedure TIpHtmlFrame.OpenRelativeURL(const URL: string);
var
  S : TStream;
  St, ResourceType : string;
  IsImage : Boolean;
begin
  InOpen := True;
  try
    if Assigned(FDataProvider) then
      St := FDataProvider.BuildURL(FCurURL, URL)
    else
      St := IpUtils.BuildURL(FCurURL, URL);

    if FDataProvider = nil then
      raise EIpHtmlException.Create(SHtmlNoDataProvider);
    if not FDataProvider.DoCheckURL(St, ResourceType) then
      raise EIpHtmlException.Create(SHtmlResUnavail + St);
    IsImage := False;
    S := nil;
    if pos('image/', LowerCase(ResourceType)) <> 0 then begin
      IsImage := True;
      S := BuildImagePage(St);
    end else

    if Pos('text/', LowerCase(ResourceType)) = 0 then begin
      FViewer.FHotURL := St;
      FViewer.DoHotClick;
      Exit;
    end;
    FCurURL := St;
    FCurAnchor := '';
    InternalFreeFrames;
    //Memory comsumption is too high without free
    FHtml := TIpHtml.Create;
    InitHtml;
    //see above
    if FDataProvider <> nil then begin
      if not IsImage then
        S := FDataProvider.DoGetHtmlStream(FCurURL, PostData);
      if S <> nil then
        try
          FHtml.FCurURL := FCurURL;
          FHtml.LoadFromStream(S);
          InternalCreateFrames;
        finally
          S.Free;
        end;
    end;
  finally
    InOpen := False;
  end;
end;

procedure TIpHtmlFrame.FramePanelResize(Sender: TObject);
begin
  AlignPanels;
end;

procedure TIpHtmlFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := FHtml.FindElement(URL);
  FCurAnchor := '';
  if E <> nil then begin
    E.MakeVisible;
    FCurAnchor := '#'+URL;
  end else
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlFrame.Home;
begin
  if FHtml <> nil then
    FHtml.Home;
end;

function TIpHtmlFrame.FindFrame(const FrameName: string): TIpHtmlFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, FName) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to Pred(FFrameCount) do begin
      Result := FFrames[i].FindFrame(FrameName);
      if Result <> nil then
        Exit;
    end;
  end;
end;

procedure TIpHtmlFrame.Get(Sender: TIpHtml; const URL: string);
begin
  FViewer.GetURL := URL;
  FViewer.PostURL := '';
  FViewer.PostData := nil;
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

procedure TIpHtmlFrame.Post(Sender: TIpHtml; const URL: string;
  FormData: TIpFormDataEntity);
begin
  FViewer.GetURL := '';
  FViewer.PostURL := URL;
  FViewer.PostData := FormData;
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

function TIpHtmlFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if FHtml = nil then
    Result := False
  else
    if FHtml.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlFrame.CopyToClipboard;
var
  i : Integer;
begin
  if FHtml <> nil then
    if FHtml.HaveSelection then
      FHtml.CopyToClipboard
    else begin
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          FFrames[i].CopyToClipboard;
          Exit;
        end;
    end;
end;

procedure TIpHtmlFrame.SelectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.SelectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].SelectAll;
  end;
end;

procedure TIpHtmlFrame.DeselectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.DeselectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].DeselectAll;
  end;
end;

procedure TIpHtmlFrame.IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
  Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
var
  MW, MH, W, H : Integer;
  Scroll : Boolean;
  NewFrame : TIpHtmlFrame;
begin
  Control := TPanel.Create(Parent);
  Pnl[FFrameCount] := TPanel(Control);
  TPanel(Control).BevelOuter := bvNone;
  case Frame.Width.LengthType of
  hlAbsolute :
    W := Frame.Width.LengthValue;
  else
  {hlUndefined,
  hlPercent :}
    begin
      if Frame.Width.LengthType = hlUndefined then
        W := Parent.ClientWidth
      else
        W := round(Frame.Width.LengthValue * Parent.ClientWidth / 100);
    end;
  end;
  case Frame.Height.LengthType of
  hlAbsolute :
    H := Frame.Height.LengthValue;
  else
  {hlUndefined,
  hlPercent :}
    begin
      if Frame.Height.LengthType = hlUndefined then
        H := Parent.ClientHeight
      else
        H := round(Frame.Height.LengthValue * Parent.ClientHeight / 100);
    end;
  end;
  TPanel(Control).SetBounds(0, 0, W, H);
  TPanel(Control).Parent := Parent;
  TPanel(Control).FullRepaint := False;
  case Frame.Scrolling of
  hfsAuto, hfsYes :
    Scroll := True;
  else //hfsNo :
    Scroll := False;
  end;
  if Frame.FrameBorder <> 0 then begin
    TPanel(Control).BorderStyle := bsSingle;
    TPanel(Control).BorderWidth := Frame.FrameBorder;
  end;

  if Frame.MarginWidth <> -1 then
    MW := Frame.MarginWidth
  else
    MW := FViewer.MarginWidth;
  if Frame.MarginHeight <> -1 then
    MH:= Frame.MarginHeight
  else
    MH := FViewer.MarginHeight;

  NewFrame :=
    TIpHtmlFrame.Create(FViewer, TCustomPanel(Control), FDataProvider,
      FViewer.FlagErrors, not Scroll, MW, MH);
  FFrames[FFrameCount] := NewFrame;
  NewFrame.FName := Frame.FName;
  Application.ProcessMessages;
  NewFrame.FCurURL := FCurURL;
  NewFrame.OpenRelativeURL(Frame.Src);
  Inc(FFrameCount);
  Frame.FFrame := NewFrame;
end;

procedure TIpHtmlFrame.SetHtml(NewHtml: TIpHtml);
begin
  InternalFreeFrames;
  FHtml := NewHtml;
  InitHtml;
  FHtml.DoneLoading := True;
  InternalCreateFrames;
end;

procedure TIpHtmlFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if FHtml <> nil then
    Enumerator(FHtml);
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlFrame.ControlClick(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlClick(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlClick2(Sender: TIpHtml; Node: TIpHtmlNodeControl;
  var cancel: boolean);
begin
  FViewer.ControlClick2(Self, Sender, Node, cancel);
end;

procedure TIpHtmlFrame.ControlOnChange(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlOnChange(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlOnEditingDone(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlOnEditingdone(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlCreate(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlCreate(Self, Sender, Node);
end;

procedure TIpHtmlFrame.Scroll(Action: TIpScrollAction);
var
  R : TRect;
  H, W : Integer;
begin
  if FHtml = nil then Exit;
  if HyperPanel = nil then Exit;
  R := FHtml.PageViewRect;
  H := R.Bottom - R.Top;
  W := R.Right - R.Left;
  case Action of
  hsaHome :
    begin
      R.Top := 0;
      R.Bottom := R.Top + H;
    end;
  hsaEnd :
    begin
      R.Bottom := FHtml.FPageRect.Bottom;
      R.Top := R.Bottom - H;
    end;
  hsaPgUp :
    begin
      OffsetRect(R, 0, -H);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaPgDn :
    begin
      OffsetRect(R, 0, H);
      if R.Bottom > FHtml.FPageRect.Bottom then begin
        R.Bottom := FHtml.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  hsaLeft :
    begin
      OffsetRect(R, -100, 0);
      if R.Left < 0 then begin
        R.Left := 0;
        R.Right := R.Left + W;
      end;
    end;
  hsaRight :
    begin
      OffsetRect(R, 100, 0);
      if R.Right > FHtml.FPageRect.Right then begin
        R.Bottom := FHtml.FPageRect.Right;
        R.Left := R.Right - W;
      end;
    end;
  hsaUp :
    begin
      OffsetRect(R, 0, -100);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaDown :
    begin
      OffsetRect(R, 0, 100);
      if R.Bottom > FHtml.FPageRect.Bottom then begin
        R.Bottom := FHtml.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  end;
  HyperPanel.ScrollInViewRaw(R);
end;

procedure TIpHtmlFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
end;

function TIpHtmlFrame.getFrame(i: integer): TIpHtmlFrame;
begin
     result := FFrames[i];
end;

{ TIpHtmlNvFrame }

procedure TIpHtmlNvFrame.InitHtml;
begin
  if FScanner.DataProvider <> nil then
    FHtml.OnGetImageX := FScanner.DataProvider.DoGetImage;
  FHtml.FlagErrors := FFlagErrors;
end;

constructor TIpHtmlNvFrame.Create(Scanner: TIpHtmlCustomScanner;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
begin
  FScanner := Scanner;
  FDataProvider := DataProvider;
  FHtml := TIpHtml.Create;
  FFlagErrors := FlagErrors;
  InitHtml;
end;

destructor TIpHtmlNvFrame.Destroy;
var
  i : Integer;
begin
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FHtml.Free;
  inherited;
end;

procedure TIpHtmlNvFrame.OpenURL(const URL: string);
begin
  OpenRelativeURL(URL);
end;

procedure TIpHtmlNvFrame.OpenRelativeURL(const {Base, }URL: string);
var
  S : TStream;
  i, C : Integer;
  ColWCount : Integer;
  St, ResourceType : string;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(FCurURL, URL)
  else
    St := IpUtils.BuildURL(FCurURL, URL);

  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);
  if CompareText(ResourceType, 'text/html') <> 0 then
    Exit;
  if CompareText(St, FCurURL) = 0 then Exit;
  FCurURL := St;
  FCurAnchor := '';
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FFrameCount := 0;
  FDataProvider.DoLeave(FHtml);
  FHtml.Clear;
  ColWCount := 0;
  if FDataProvider <> nil then begin
    S := FDataProvider.DoGetHtmlStream(FCurURL, PostData);
    if S <> nil then
      try
        FHtml.FCurURL := FCurURL;
        FHtml.LoadFromStream(S);
        if FHtml.HasFrames then begin
          C := 0;
          FFrameCount := 0;
          for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
            if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              CurFrameDef := TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]);
              FFrames[FFrameCount] :=
                TIpHtmlNvFrame.Create(FScanner, FDataProvider,
                  FScanner.FlagErrors);
              FFrames[FFrameCount].FName := CurFrameDef.Name;
              if C < ColWCount - 1 then
                Inc(C)
              else begin
                C := 0;
              end;
              Inc(FFrameCount);
            end;
          end;
          Application.ProcessMessages;
          FFrameCount := 0;
          for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
            if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              FFrames[FFrameCount].FCurURL := FCurURL;
              FFrames[FFrameCount].OpenRelativeURL({Base,}
                TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]).Src);
              Inc(FFrameCount);
            end;
          end;
        end;
      finally
        S.Free;
      end;
  end;
end;

procedure TIpHtmlNvFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := FHtml.FindElement(URL);
  FCurAnchor := '';
  if E <> nil then begin
    E.MakeVisible;
    FCurAnchor := '#'+URL;
  end else
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlNvFrame.Home;
begin
  if FHtml <> nil then
    FHtml.Home;
end;

function TIpHtmlNvFrame.FindFrame(const FrameName: string): TIpHtmlNvFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, FName) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to Pred(FFrameCount) do begin
      Result := FFrames[i].FindFrame(FrameName);
      if Result <> nil then
        Exit;
    end;
  end;
end;

function TIpHtmlNvFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if FHtml = nil then
    Result := False
  else
    if FHtml.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlNvFrame.CopyToClipboard;
var
  i : Integer;
begin
  if FHtml <> nil then
    if FHtml.HaveSelection then
      FHtml.CopyToClipboard
    else begin
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          FFrames[i].CopyToClipboard;
          Exit;
        end;
    end;
end;

procedure TIpHtmlNvFrame.SelectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.SelectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].SelectAll;
  end;
end;

procedure TIpHtmlNvFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if FHtml <> nil then
    Enumerator(FHtml);
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlNVFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
end;

function TIpHtmlNVFrame.getFrame(i: integer): TIpHtmlNVFrame;
begin
  result := FFrames[i];
end;

{ TIpHtmlCustomPanel }

procedure TIpHtmlCustomPanel.DoHotChange;
begin
  if Assigned(FHotChange) then
    FHotChange(Self);
end;

procedure TIpHtmlCustomPanel.DoHotClick;
begin
  if Assigned(FHotClick) then
    FHotClick(Self);
end;

procedure TIpHtmlCustomPanel.DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);
var
  I: Integer;
begin
  if Delta < 0 then
  begin
    for I := 1 to WheelDelta do
      Scroll(hsaDown);
  end else
  if Delta > 0 then
  begin
    for I := 1 To WheelDelta do
      Scroll(hsaUp);
  end;
end;

procedure TIpHtmlCustomPanel.HotChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  vHtml : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  vHtml := P.Hyper;
  if vHtml.HotNode <> nil then begin
    if vHtml.HotPoint.x >= 0 then
      FHotURL := TIpHtmlNodeA(vHtml.HotNode).HRef+
        '?'+IntToStr(vHtml.HotPoint.x)+','+IntToStr(vHtml.HotPoint.y)
    else
      if vHtml.HotNode is TIpHtmlNodeA then
       FHotURL := TIpHtmlNodeA(vHtml.HotNode).HRef
      else
       FHotURL := TIpHtmlNodeAREA(vHtml.HotNode).HRef;
    FHotNode := vHtml.HotNode;
    P.Cursor := crHandPoint;
  end else begin
    FHotNode := nil;
    FHotURL := '';
    P.Cursor := crDefault;
  end;
  DoHotChange;
end;

procedure TIpHtmlCustomPanel.CurElementChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  vHtml : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  vHtml := P.Hyper;
  FCurElement := vHtml.CurElement;
  if assigned(FCurElementChange) then
    FCurElementChange(Self);
end;

function TIpHtmlCustomPanel.GetTitle: string;
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FMasterFrame.FHtml.TitleNode <> nil) then
    Result := FMasterFrame.FHtml.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  Caption := '';
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
  VisitedList := TStringList.Create;
  VisitedList.Sorted := True;
  FTextColor := clBlack;
  FLinkColor := clBlue;
  FVLinkColor := clMaroon;
  FALinkColor := clRed;
  FBgColor := clWhite;
  FShowHints := True;
  FMarginWidth := 10;
  FMarginHeight := 10;
  FAllowTextSelect := True;
  FixedTypeface := 'Courier New';
  DefaultTypeFace := Graphics.DefFontData.Name;
  DefaultFontSize := 12;
  FPrintSettings := TIpHtmlPrintSettings.Create;
  FFactBAParag := 1;
  FWantTabs := True;
end;

destructor TIpHtmlCustomPanel.Destroy;
begin
  FPrintSettings.Free;
  TargetStack.Free;
  URLStack.Free;
  FMasterFrame.Free;
  FMasterFrame := nil;
  VisitedList.Free;
  inherited;
end;

procedure TIpHtmlCustomPanel.EraseBackground(DC: HDC);
begin
  //
end;

procedure TIpHtmlCustomPanel.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomPanel.MakeAnchorVisible(const Name: string);
begin
  if FMasterFrame <> nil then
    FMasterFrame.MakeAnchorVisible(Name)
end;

procedure TIpHtmlCustomPanel.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlFrame;
begin
  if HRef = '' then
    Exit;
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end
  else begin
    if FMasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(FMasterFrame.FHtml.FCurURL, HRef)
      else
        URL := IpUtils.BuildURL(FMasterFrame.FHtml.FCurURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL = '' then begin
    if FMasterFrame <> nil then
      Push('', RelURL);
  end
  else  begin
    if VisitedList.IndexOf(BaseURL) = -1 then
      VisitedList.Add(BaseURL);
    if (Target <> '') and (FMasterFrame <> nil) then
      TargetFrame := FMasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if FMasterFrame <> nil then
        Push('', FMasterFrame.FCurURL + FMasterFrame.FCurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);
      if (FMasterFrame = nil)
      or ((FMasterFrame <> nil) and (not FMasterFrame.IsExternal(URL))) then begin
        if (FMasterFrame <> nil)
        and (FMasterFrame.FHtml <> nil) then
          FDataProvider.DoLeave(FMasterFrame.FHtml);
        FMasterFrame.Free;
        FMasterFrame := nil;
        Application.ProcessMessages;
        FMasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
          MarginWidth, MarginHeight);
        // LazDebug try
          FMasterFrame.OpenURL(URL, False);
        { LazDebug except
          FMasterFrame.Free;
          FMasterFrame := nil;
          raise;
        end;}
        {FCurURL := URL;}
      end;
    end else begin
      Push(Target, TargetFrame.FCurURL +  TargetFrame.FCurAnchor);
      TargetFrame.OpenURL(BaseURL, False);
    end;
  end;
  if RelURL <> '' then
    FMasterFrame.MakeAnchorVisible(RelURL)
  else
    if FMasterFrame <> nil then
      FMasterFrame.Home;
  if assigned(FDocumentOpen) then
    FDocumentOpen(Self);
end;

procedure TIpHtmlCustomPanel.HotClick(Sender: TObject);
var
  HRef : string;
  Target : string;
begin
  if TIpHtml(Sender).HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeA(TIpHtml(Sender).HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).Target;
  end;
  if (FDataProvider <> nil)
  and FDataProvider.CanHandle(HRef) then
    InternalOpenURL(Target, HRef)
  else
    DoHotClick;
end;

procedure TIpHtmlCustomPanel.GoBack;
begin
  if (URLStack.Count > 0) then begin
    {$IFDEF IP_LAZARUS}
    if URLStack.Count >= URLStack.count then Stp := URLStack.Count - 1;
    if URLStack.Count > 0 then begin
      InternalOpenURL(TargetStack[Stp], URLStack[Stp]);
      Dec(Stp);
    end;
    {$ELSE}
    InternalOpenURL(TargetStack[Stp], URLStack[Stp]);
    Dec(Stp);
    {$ENDIF}
  end;
end;

function TIpHtmlCustomPanel.canGoBack : boolean;
begin
  Result := (URLStack.Count > 0);
end;

procedure TIpHtmlCustomPanel.GoForward;
begin
  if Stp < URLStack.Count - 1 then begin
    InternalOpenURL(TargetStack[Stp + 1], URLStack[Stp + 1]);
    Inc(Stp);
  end;
end;

function TIpHtmlCustomPanel.canGoForward : boolean;
begin
  Result := (Stp < URLStack.Count - 1);
end;

procedure TIpHtmlCustomPanel.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then Exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  //debugln(['TIpHtmlCustomPanel.Notification ',DbgSName(Self),' ',dbgs(Pointer(Self)),' AComponent=',DbgSName(AComponent),' ',dbgs(Pointer(AComponent))]);
  if (Operation = opRemove) then
    if (AComponent = DataProvider) then begin
      DataProvider := nil;
    end;
  inherited Notification(AComponent, Operation);
end;

procedure TIpHtmlCustomPanel.Paint;
var
  Sz: TSize;
begin
  if csDesigning in ComponentState then begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Pen.Color := clWhite;
    Sz := Canvas.TextExtent('Html');
    Canvas.Polygon([
      Point(0,4),
      Point(0, Height - 5),
      Point(Width div 2 - Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(Width - 1,4),
      Point(Width - 1, Height - 5),
      Point(Width div 2 + Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(2, 4),
      Point(Width - 3, 4),
      Point(Width div 2, Height div 2 - Sz.cy div 2)]);
    Canvas.Polygon([
      Point(2, Height - 4),
      Point(Width - 3, Height - 4),
      Point(Width div 2, Height div 2 + Sz.cy div 2)]);
    Canvas.Brush.Color := clRed;
    Canvas.Pen.Color := clBlack;
    Canvas.Ellipse(
           Width div 2 - Sz.cx, Height div 2 - Sz.cy,
           Width div 2 + Sz.cx, Height div 2 + Sz.cy);
    Canvas.TextOut(Width div 2 - Sz.cx div 2, Height div 2 - Sz.cy div 2, 'Html');
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
  end;
end;

procedure TIpHtmlCustomPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if (FMasterFrame = nil)
  or (FMasterFrame.FHtml = nil)
  or (not FMasterFrame.FHtml.CanPaint) then
    if not (csDesigning in ComponentState) then
      FillRect(Message.DC, ClientRect, Brush.Reference.Handle);
  Message.Result := 1;
end;

procedure TIpHtmlCustomPanel.CMIpHttpGetRequest(var Message: TMessage);
var
  FB : TIpHtmlFrame;
begin
  FB := TIpHtmlFrame(Message.lParam);
  if PostData <> nil then begin
    FB.PostData := PostData;
    FB.OpenRelativeURL(PostURL);
    {$IFNDEF HtmlWithoutHttp}
    PostData.Free;
    PostData := nil;
    {$ENDIF}
  end else
    FB.OpenRelativeURL(GetURL);
  if assigned(FDocumentOpen) then
    FDocumentOpen(Self);
end;

procedure TIpHtmlCustomPanel.ClientClick(Sender: TObject);
begin
  Click;
end;

function TIpHtmlCustomPanel.HaveSelection: Boolean;
begin
  Result := (FMasterFrame <> nil) and (FMasterFrame.HaveSelection);
end;

procedure TIpHtmlCustomPanel.SelectAll;
begin
  if FMasterFrame <> nil then begin
    FMasterFrame.SelectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.DeselectAll;
begin
  if FMasterFrame <> nil then begin
    FMasterFrame.DeselectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.CopyToClipboard;
begin
  if FMasterFrame <> nil then
    FMasterFrame.CopyToClipboard;
end;

procedure TIpHtmlCustomPanel.SetHtml(NewHtml: TIpHtml);
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FDataProvider <> nil) then
    FDataProvider.DoLeave(FMasterFrame.FHtml);
  FMasterFrame.Free;
  FMasterFrame := nil;
  FMasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
    MarginWidth, MarginHeight);
  // LazDebug try
    if NewHtml <> nil then  begin
      NewHtml.FactBAParag := FactBAParag;
      NewHtml.BgColor := BgColor;
      NewHtml.FixedTypeface := FixedTypeface;
      NewHtml.DefaultTypeFace := DefaultTypeFace;
      NewHtml.DefaultFontSize := FDefaultFontSize;
      FMasterFrame.SetHtml(NewHtml);
    end;
  { LazDebug
  except
    FMasterFrame.Free;
    FMasterFrame := nil;
    raise;
  end;}
end;

procedure TIpHtmlCustomPanel.SetHtmlFromStr(NewHtml: string);
var
   iphtml: TIpHtml;
   strm: TStringStream;
begin
     iphtml:= TIpHtml.Create;
     strm:= TStringStream.Create(NewHtml);
     iphtml.LoadFromStream(strm);
     SetHtml(iphtml);
     strm.Free;
end;

procedure TIpHtmlCustomPanel.SetHtmlFromStream(NewHtml: TStream);
var
   iphtml: TIpHtml;
begin
     iphtml:= TIpHtml.Create;
     iphtml.LoadFromStream(NewHtml);
     SetHtml(iphtml);
end;

procedure TIpHtmlCustomPanel.URLCheck(Sender: TIpHtml; const URL: string;
  var Visited: Boolean);
begin
  Visited := VisitedList.IndexOf(URL) <> -1;
end;

procedure TIpHtmlCustomPanel.ReportURL(Sender: TIpHtml; const URL: string);
begin
  if (FDataProvider <> nil) then
    FDataProvider.DoReference(URL);
end;

procedure TIpHtmlCustomPanel.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if FMasterFrame <> nil then
    FMasterFrame.EnumDocuments(Enumerator);
end;

procedure TIpHtmlCustomPanel.ControlClick(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlClick2(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl; var pCancel: boolean);
begin
  if assigned(FControlClick2) then
    FControlClick2(Self, pFrame, pHtml, pNode, pCancel);
end;

procedure TIpHtmlCustomPanel.ControlOnChange(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlOnChange) then
    FControlOnChange(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlOnEditingDone(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlOnEditingDone) then
    FControlOnEditingDone(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlCreate(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, pFrame, pHtml, pNode);
end;

function TIpHtmlCustomPanel.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomPanel.Stop;
begin
  if assigned(FMasterFrame) then
    FMasterFrame.Stop;
end;

{$IF defined(VERSION4) and not defined(IP_LAZARUS)}
procedure TIpHtmlCustomPanel.MouseWheelHandler(var Message: TMessage);
begin
  inherited MouseWheelHandler(Message);
  with Message do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)), HIWORD(wParam), LOWORD(lParam), HIWORD(lParam));
end;
{$ENDIF}

function TIpHtmlCustomPanel.GetPrintPageCount: Integer;
begin
  if Assigned(FMasterFrame) and Assigned(FMasterFrame.HyperPanel) then
    Result := FMasterFrame.HyperPanel.GetPrintPageCount
  else
    Result := 0;
end;

procedure TIpHtmlCustomPanel.Print(FromPg, ToPg: LongInt);
begin
  if Assigned(FMasterFrame) then
    FMasterFrame.HyperPanel.PrintPages(FromPg, ToPg);
end;

procedure TIpHtmlCustomPanel.PrintPreview;
begin
  {$IFDEF IP_LAZARUS}
  if not assigned(printer) then begin
    raise exception.create(
      'Printer has not been assigned, checkout that package'#13+
      'Printer4lazarus.lpk has been installed and OSPrinters'#13+
      'or PrintDialog is in uses clause of main unit');
  end;
  {$ENDIF}
  if Assigned(FMasterFrame) then
    FMasterFrame.HyperPanel.PrintPreview;
end;

function TIpHtmlCustomPanel.GetContentSize: TSize;
begin
  if FMasterFrame <> nil then
  begin
    with FMasterFrame.FHtml.FPageRect do
    begin
      Result.cx := Right - Left;
      Result.cy := Bottom - Top;
    end;
  end
  else
    Result := Size(0, 0);
end;

procedure TIpHtmlCustomPanel.Scroll(Action: TIpScrollAction);
begin
  if FMasterFrame <> nil then
    FMasterFrame.Scroll(Action);
end;                    

procedure TIpHtmlCustomPanel.WMGetDlgCode(var Msg: TMessage);
begin
  { we want 'em all!  For Lazarus: Then use OnKeyDown! }
  Msg.Result := DLGC_WANTALLKEYS +
                DLGC_WANTARROWS +
                DLGC_WANTCHARS +
                {$IFNDEF IP_LAZARUS}
                DLGC_WANTMESSAGE +
                {$ENDIF}
                DLGC_WANTTAB
end;

function TIpHtmlCustomPanel.GetVersion : string;
begin
  Result := IpShortVersion;
end;

function TIpHtmlCustomPanel.GetCurUrl: string;
begin
  Result := FMasterFrame.FCurURL;
end;

procedure TIpHtmlCustomPanel.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;

procedure TIpHtmlCustomPanel.SetDefaultTypeFace(const Value: string);
begin
  if FDefaultTypeFace<>Value then begin
    FDefaultTypeFace := Value;
    if (FMasterFrame<>nil)and(FMasterFrame.FHtml<>nil) then begin
      FMasterFrame.FHtml.DefaultTypeFace := FDefaultTypeFace;
      Invalidate;
    end;
  end;
end;

procedure TIpHtmlCustomPanel.SetDefaultFontSize(const Value: integer);
begin
  if FDefaultFontSize<>Value then begin
    FDefaultFontSize := Value;
    if (FMasterFrame<>nil)and(FMasterFrame.FHtml<>nil) then begin
      FMasterFrame.FHtml.DefaultFontSize := FDefaultFontSize;
      Invalidate;
    end;
  end;
end;

procedure TIpHtmlCustomPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  r: TRect;
begin
  //debugln(['TIpHtmlCustomPanel.CalculatePreferredSize ',DbgSName(Self)]);
  r:=Rect(0,0,0,0);
  if (FMasterFrame<>nil) and (FMasterFrame.HyperPanel<>nil)
  and (FMasterFrame.HyperPanel.Hyper<>nil) then
    r:=FMasterFrame.HyperPanel.Hyper.GetPageRect(Canvas, 0, 0);
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  if PreferredWidth<r.Right-r.Left then
    PreferredWidth:=r.Right-r.Left;
  if PreferredHeight<r.Bottom-r.Top then
    PreferredHeight:=r.Bottom-r.Top;
end;

procedure TIpHtmlCustomPanel.SetFactBAParag(const Value: Real);
var
  V: Real;
begin
  V := Value;
  if V > 2 then
    V := 2
  else if  V < 0 then
    V := 0;
  FFactBAParag := V;
end;

procedure TIpHtmlCustomPanel.SetDataProvider(const AValue: TIpAbstractHtmlDataProvider);
begin
  if FDataProvider=AValue then exit;
  //debugln(['TIpHtmlCustomPanel.SetDataProvider Old=',DbgSName(FDataProvider),' ',dbgs(Pointer(FDataProvider)),' New=',DbgSName(AValue),' ',dbgs(Pointer(AValue))]);
  FDataProvider:=AValue;
  if FDataProvider<>nil then FreeNotification(FDataProvider);
end;

function TIpHtmlCustomPanel.FactBAParagNotIs1: Boolean;
begin
  Result := FactBAParag <> 1;
end;

function TIpHtmlCustomPanel.GetVScrollPos: Integer;
begin
  if  FMasterFrame <> nil
  then  Result := FMasterFrame.HyperPanel.VScroll.Position
  else  Result := -1;
end;

procedure TIpHtmlCustomPanel.SetVScrollPos(const Value: Integer);
begin
  if  (FMasterFrame <> nil) and (Value >= 0)
  then  FMasterFrame.HyperPanel.VScroll.Position := Value;
end;

{ TIpHtmlCustomScanner }

function TIpHtmlCustomScanner.GetTitle: string;
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FMasterFrame.FHtml.TitleNode <> nil) then
    Result := FMasterFrame.FHtml.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomScanner.Create(AOwner: TComponent);
begin
  inherited;
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
end;

destructor TIpHtmlCustomScanner.Destroy;
begin
  TargetStack.Free;
  URLStack.Free;
  FMasterFrame.Free;
  FMasterFrame := nil;
  inherited;
end;

procedure TIpHtmlCustomScanner.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomScanner.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlNvFrame;
begin
  if HRef = '' then
    Exit;
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end else begin
    if FMasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(FMasterFrame.FHtml.FCurURL, HRef)
      else
        URL := IpUtils.BuildURL(FMasterFrame.FHtml.FCurURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL <> '' then begin
    if (Target <> '') and (FMasterFrame <> nil) then
      TargetFrame := FMasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if FMasterFrame <> nil then
        Push('', FMasterFrame.FCurURL + FMasterFrame.FCurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);
      if (FMasterFrame <> nil)
      and (FMasterFrame.FHtml <> nil) then
        FDataProvider.DoLeave(FMasterFrame.FHtml);
      FMasterFrame.Free;
      FMasterFrame := nil;
      Application.ProcessMessages;
      FMasterFrame := TIpHtmlNVFrame.Create(Self, DataProvider, FlagErrors);
      // LazDebug try
        FMasterFrame.OpenURL(URL);
      { LazDebug except
        FMasterFrame.Free;
        FMasterFrame := nil;
        raise;
      end;}
      FCurURL := URL;
    end else begin
      Push(Target, TargetFrame.FCurURL +  TargetFrame.FCurAnchor);
      TargetFrame.OpenURL(BaseURL);
    end;
  end;
  if RelURL <> '' then
    FMasterFrame.MakeAnchorVisible(RelURL)
  else
    FMasterFrame.Home;
end;

procedure TIpHtmlCustomScanner.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then Exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomScanner.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if FMasterFrame <> nil then
    FMasterFrame.EnumDocuments(Enumerator);
end;

function TIpHtmlCustomScanner.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomScanner.Stop;
begin
  if assigned(FMasterFrame) then
    FMasterFrame.Stop;
end;

function TIpHtmlCustomScanner.GetVersion : string;
begin
  Result := IpShortVersion;
end;

procedure TIpHtmlCustomScanner.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;

{$IFDEF IP_LAZARUS}
function LazFlatSB_GetScrollInfo(hWnd: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): BOOL; stdcall;
begin
  Result:=LCLIntf.GetScrollInfo(HWnd,BarFlag,ScrollInfo);
end;
  
function LazFlatSB_GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;
begin
  Result:=LCLIntf.GetScrollPos(HWnd,nBar);
end;

function LazFlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer;
  bRedraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollPos(HWnd,nBar,nPos,bRedraw);
end;

function LazFlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer;
  p4: Bool): Bool; stdcall;
begin
  // ToDo
  Result:=true;
end;

function LazFlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer;
  const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollInfo(HWnd,BarFlag,ScrollInfo,Redraw);
end;
{$ENDIF}


procedure InitScrollProcs;
{$IFNDEF IP_LAZARUS}
var
  ComCtl32: THandle;
{$ENDIF}
begin
  {$IFDEF IP_LAZARUS}
  @FlatSB_GetScrollInfo := @LazFlatSB_GetScrollInfo;
  @FlatSB_GetScrollPos :=  @LazFlatSB_GetScrollPos;
  @FlatSB_SetScrollPos :=  @LazFlatSB_SetScrollPos;
  @FlatSB_SetScrollProp := @LazFlatSB_SetScrollProp;
  @FlatSB_SetScrollInfo := @LazFlatSB_SetScrollInfo;
  {$ELSE}
  ComCtl32 := GetModuleHandle('comctl32.dll');
  @FlatSB_GetScrollInfo := GetProcAddress(ComCtl32, 'FlatSB_GetScrollInfo');
  @FlatSB_GetScrollPos := GetProcAddress(ComCtl32, 'FlatSB_GetScrollPos');
  @FlatSB_SetScrollPos := GetProcAddress(ComCtl32, 'FlatSB_SetScrollPos');
  @FlatSB_SetScrollProp := GetProcAddress(ComCtl32, 'FlatSB_SetScrollProp');
  @FlatSB_SetScrollInfo := GetProcAddress(ComCtl32, 'FlatSB_SetScrollInfo');
  {$ENDIF}
end;


{ TIntArr }

destructor TIntArr.Destroy;
begin
  inherited;
  Freemem(InternalIntArr);
end;

function TIntArr.GetValue(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= IntArrSize) then
    Result := 0
  else
    Result := InternalIntArr^[Index];
end;

procedure TIntArr.SetValue(Index, Value: Integer);
var
  {$IFDEF IP_LAZARUS}
  p: ^Integer;
  {$ELSE}
  Tmp: PInternalIntArr;
  {$ENDIF}
  NewSize: Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil}
      ReallocMem(InternalIntArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalIntArr);
      Inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize := NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalIntArr^, Tmp^, IntArrSize * sizeof(Integer));
      IntArrSize := NewSize;
      {Inc(IntArrSize, NewSize);}
      Freemem(InternalIntArr);
      InternalIntArr := Tmp;
      {$ENDIF}
    end;
    InternalIntArr^[Index] := Value;
  end;
end;

{ TRectArr }

destructor TRectArr.Destroy;
begin
  inherited;
  Freemem(InternalRectArr);
end;

function TRectArr.GetValue(Index: Integer): PRect;
begin
  Assert(Self <> nil);
  if (Index < 0) or (Index >= IntArrSize) then
    Result := nil
  else
    Result := InternalRectArr^[Index];
end;

procedure TRectArr.SetValue(Index: Integer; Value: PRect);
var
  {$IFNDEF IP_LAZARUS}
  Tmp: PInternalRectArr;
  {$ELSE}
  P: Pointer;
  {$ENDIF}
  NewSize: Integer;
begin
  Assert(Self <> nil);
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectArr,NewSize * sizeof(PtrInt));
      P := pointer(InternalRectArr);
      Inc(P, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalRectArr^, Tmp^, IntArrSize * sizeof(Integer));
      Inc(IntArrSize, NewSize);
      Freemem(InternalRectArr);
      InternalRectArr := Tmp;
      {$ENDIF}
    end;
    InternalRectArr^[Index] := Value;
  end;
end;

{ TRectRectArr }

procedure TRectRectArr.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= 0) and (Index < IntArrSize) then begin
    Value[Index].Free;
    for i := 1 to IntArrSize - 1 do
      InternalRectRectArr[i-1] := InternalRectRectArr[i];
    InternalRectRectArr[IntArrSize - 1] := nil;
  end;
end;

destructor TRectRectArr.Destroy;
var
  i: Integer;
begin
  inherited;
  for i := 0 to IntArrSize - 1 do
    Delete(i);
  if InternalRectRectArr <> nil then
    Freemem(InternalRectRectArr);
end;

function TRectRectArr.GetValue(Index: Integer): TRectArr;
var
  {$IFNDEF IP_LAZARUS}
  Tmp: PInternalRectRectArr;
  {$ELSE}
  P: ^Pointer;
  {$ENDIF}
  NewSize: Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectRectArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalRectRectArr);
      Inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalRectRectArr^, Tmp^, IntArrSize * sizeof(Integer));
      Inc(IntArrSize, NewSize);
      Freemem(InternalRectRectArr);
      InternalRectRectArr := Tmp;
      {$ENDIF}
    end;
    Result := InternalRectRectArr^[Index];
    if Result = nil then begin
      Result := TRectArr.Create;
      InternalRectRectArr^[Index] := Result;
    end;
  end else
    Result := nil;
end;

{ TIpHtmlPrintSettings }

constructor TIpHtmlPrintSettings.Create;
begin
  inherited;
  FMarginLeft := DEFAULT_PRINTMARGIN;
  FMarginTop := DEFAULT_PRINTMARGIN;
  FMarginRight := DEFAULT_PRINTMARGIN;
  FMarginBottom := DEFAULT_PRINTMARGIN;
end;

destructor TIpHtmlPrintSettings.Destroy;
begin
  inherited;
end;

{ TIpHtmlNodeTH }

constructor TIpHtmlNodeTH.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'th';
  {$ENDIF}
end;

{ TIpHtmlNodeTD }

constructor TIpHtmlNodeTD.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'td';
  {$ENDIF}
end;

{ TIpHtmlNodeCAPTION }

constructor TIpHtmlNodeCAPTION.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  {$IFDEF IP_LAZARUS}
  FElementName := 'caption';
  {$ENDIF}
end;

initialization
  InitScrollProcs;
end.

