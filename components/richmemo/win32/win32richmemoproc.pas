{
 win32richmemoproc.pas 
 
 Author: Dmitry 'skalogryz' Boyarintsev 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Win32RichMemoProc; 

{$mode objfpc}{$H+}

interface

uses
  // windows units
  Windows,richedit, 
  // RTL units  
  Classes, SysUtils, 
  // LCL units
  Graphics,
  // RichMemoUnits
  RichMemo, WSRichMemo, RichMemoUtils,
  // Win32 widgetset units  
  win32proc, ActiveX, ComObj;

const
  IID_IRichEditOle: TGUID = '{00020D00-0000-0000-C000-000000000046}';
  IID_IRichEditOleCallback: TGUID = '{00020D03-0000-0000-C000-000000000046}';
  CLSID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

const
  OLERENDER_NONE    = 0;
  OLERENDER_DRAW    = 1;
  OLERENDER_FORMAT  = 2;
  OLERENDER_ASIS    = 3;

const
  REO_GETOBJ_NO_INTERFACES	= 0;
  REO_GETOBJ_POLEOBJ	= 1;
  REO_GETOBJ_PSTG	 = 2;
  REO_GETOBJ_POLESITE	 = 4;
  REO_GETOBJ_ALL_INTERFACES	= 7;
  REO_CP_SELECTION  = -1;
  REO_IOB_SELECTION = -1;
  REO_IOB_USE_CP = -2;

  REO_NULL	          = $00000000;

  REO_RESIZABLE	      = $00000001;
  REO_BELOWBASELINE	  = $00000002;
  REO_INVERTEDSELECT	= $00000004;
  REO_DYNAMICSIZE		  = $00000008;
  REO_BLANK	          = $00000010;
  REO_DONTNEEDPALETTE	= $00000020;

  // Rich edit 3.0
  REO_OWNERDRAWSELECT = $00000040;
  REO_CANROTATE       = $00000080;
  REO_ALIGNTORIGHT    = $00000100;
  REO_WRAPTEXTAROUND  = $00000200;
  REO_USEASBACKGROUND = $00000400;

  REO_READWRITEMASK	  = $000007FF;

  REO_LINKAVAILABLE	  = $00800000;
  REO_GETMETAFILE	    = $00400000;
  REO_HILITED	        = $01000000;
  REO_INPLACEACTIVE	  = $02000000;
  REO_OPEN	          = $04000000;
  REO_SELECTED			  = $08000000;
  REO_STATIC          =	$40000000;
  REO_LINK	          = $80000000;

  RECO_PASTE = 0;
  RECO_DROP	 = 1;
  RECO_COPY	 = 2;
  RECO_CUT	 = 3;
  RECO_DRAG	 = 4;



type
  TREOBJECT = packed record
    cbStruct  : DWORD;
    cp        : LONG;
    clsid     : CLSID;
    poleobj   : IOLEOBJECT;
    pstg      : ISTORAGE;
    polesite  : IOLECLIENTSITE;
    sizel     : SIZEL;
    dvaspect  : DWORD;
    dwFlags   : DWORD;
    dwUser    : DWORD;
  end;

type
  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    // *** IRichEditOle methods ***
    function GetClientSite(out clientSite: IOleClientSite): HRESULT; stdcall;
    function GetObjectCount: LongInt; stdcall;
    function GetLinkCount: LongInt; stdcall;
    function GetObject(iob: LongInt; out ReObject: TReObject;
      dwFlags: DWORD): HRESULT; stdcall;
    function InsertObject(var ReObject: TReObject): HRESULT; stdcall;
    function ConvertObject(iob: LongInt; const clsidNew: TCLSID;
      lpStrUserTypeNew: LPCSTR): HRESULT; stdcall;
    function ActivateAs(const clsid, clsidAs: TCLSID): HRESULT; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HRESULT; stdcall;
    function SetLinkAvailable(iob: LongInt; fAvailable: BOOL): HRESULT; stdcall;
    function SetDvaspect(iob: LongInt; dvaspect: DWORD): HRESULT; stdcall;
    function HandsOffStorage(iob: LongInt): HRESULT; stdcall;
    function SaveCompleted(iob: LongInt; const stg: IStorage): HRESULT; stdcall;
    function InPlaceDeactivate: HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function ImportDataObject(const dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HRESULT; stdcall;
  end;

  IRichEditOleCallback = interface(IUnknown)
    ['{00020D03-0000-0000-C000-000000000046}']
    // *** IRichEditOleCallback methods ***
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: LongInt): HRESULT; stdcall;
    function DeleteObject(const oleobj: IOleObject): HRESULT; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

type
  { TRichEditManager }

  TRichEditManager = class(TObject)
  public
    class function SetEventMask(RichEditWnd: Handle; eventmask: integer): Integer;

    class function GetTextLength(RichEditWnd: Handle): Integer;
    class function SetDefaultTextStyle(RichEditWnd: Handle; Params: TIntFontParams): Boolean; virtual;
    class function SetSelectedTextStyle(RichEditWnd: Handle; Params: TIntFontParams;
      useMask: Boolean = false; AModifyMask: TTextModifyMask = []): Boolean; virtual;
    class function GetSelectedTextStyle(RichEditWnd: Handle; var Params: TIntFontParams): Boolean; virtual;
    class procedure SetTextUIStyle(RichEditWnd: Handle; const ui: TTextUIParam); virtual;
    class function GetTextUIStyle(RichEditWnd: Handle; var ui: TTextUIParam): Boolean; virtual;

    class function GetStyleRange(RichEditWnd: Handle; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; virtual;
    class procedure GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); virtual;      
    class procedure SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); virtual;

    // WARNING: GetSelRange, is causing changes in Selection!
    class procedure GetSelRange(RichEditWnd: Handle; var sr: TCHARRANGE); virtual;
    class procedure SetSelRange(RichEditWnd: Handle; const sr: TCHARRANGE); virtual;

    class procedure SetHideSelection(RichEditWnd: Handle; AValue: Boolean); virtual;
    class function LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; virtual;
    class function SaveRichText(RichEditWnd: Handle; ADst: TStream; extraflags: Integer = 0): Boolean; virtual;

    class procedure SetText(RichEditWnd: Handle; const Text: WideString; TextStart, ReplaceLength: Integer); virtual;
    class function GetTextW(RichEditWnd: Handle; inSelection: Boolean): WideString; virtual;
    class function GetTextA(RichEditWnd: Handle; inSelection: Boolean): AnsiString; virtual;
    class function GetTextUtf8(RichEditWnd: Handle; inSelection: Boolean): string;

    class procedure GetPara2(RichEditWnd: Handle; TextStart: Integer; var para: PARAFORMAT2); virtual;
    class procedure SetPara2(RichEditWnd: Handle; TextStart, TextLen: Integer; const para: PARAFORMAT2); virtual;
    // the ugly Find() overload, might go away eventually
    class function Find(RichEditWnd: THandle; const ANiddle: WideString; const ASearch: TIntSearchOpt; var TextLen: Integer): Integer; virtual; overload;
    class function Find(RichEditWnd: THandle; const ANiddle: WideString; const ASearch: TIntSearchOpt): Integer; overload;
    class procedure GetParaRange(RichEditWnd: Handle; TextStart: integer; var para: TParaRange); virtual;

    class procedure GetScroll(RichEditWnd: Handle; out pt: TPoint); virtual;
    class procedure SetScroll(RichEditWnd: Handle; const pt: TPoint); virtual;

    class function LinkNotifyToInfo(RichEditWnd: Handle; const LinkNotify: TENLINK;
      var LinkInfo: TLinkMouseInfo): Boolean; virtual;
  end;
  TRichManagerClass = class of TRichEditManager;

  { TRichEditManagerWinXP }

  TRichEditManagerWinXP = class(TRichEditManager)
    class procedure SetTextUIStyle(RichEditWnd: Handle; const ui: TTextUIParam); override;
    class function LinkNotifyToInfo(RichEditWnd: Handle; const LinkNotify: TENLINK;
      var LinkInfo: TLinkMouseInfo): Boolean; override;
  end;
                     
var
  RichEditManager : TRichManagerClass = nil;

function InitRichEdit: Boolean;
function GetRichEditClass: AnsiString;
procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
function FontStylesToEffects(Styles: TFontStyles): LongWord;
function EffectsToFontStyles(Effects: LongWord): TFontStyles;

const
  GT_SELECTION = 2;
  CP_UNICODE   = 1200;
  HardBreak    = #13;

  CFE_PROTECTED = $00000010;
  CFE_LINK      = $00000020;
  CFM_BACKCOLOR = $04000000;
  CFE_AUTOBACKCOLOR = CFM_BACKCOLOR;

  ST_DEFAULT   = $00000000;
  ST_KEEPUNDO  = $00000001;
  ST_SELECTION = $00000002;
  ST_NEWCHARS  = $00000004;
  ST_UNICODE   = $00000008;

const
  PFNS_PAREN      = $0000;
  PFNS_PARENS     = $0100;
  PFNS_PERIOD     = $0200;
  PFNS_PLAIN      = $0300;
  PFNS_NONUMBER   = $0400;
  PFNS_NEWNUMBER  = $8000;
  PFNS_SOMESEPCHAR = PFNS_PARENS or PFNS_PERIOD or PFNS_PLAIN;

const
  // this is the list of CHARFORMAT attributes that RichMemo supports
  CFM_RICHMEMO_ATTRS = CFM_COLOR or CFM_FACE or CFM_SIZE or CFM_EFFECTS
                       or CFM_SUBSCRIPT or CFM_SUBSCRIPT or CFM_BACKCOLOR;

type
  TSetTextEx = packed record
    flags    : DWORD;
    codepage : UINT;
  end;

implementation

const
  GlobalRichClass : AnsiString = '';
  UnicodeEnabledOS : Boolean = true; // todo: implement it to work with Windows 9x, if necessary
  
const
  TwipsInFontSize = 20; // see MSDN for CHARFORMAT Structure CFM_SIZE
  
function GetRichEditClass: AnsiString;
begin
  Result := GlobalRichClass;
end;  
 
function InitRichEdit: Boolean;
begin
  if GlobalRichClass = '' then begin
    if LoadLibrary('Msftedit.dll') <> 0 then begin
      GlobalRichClass := 'RichEdit50W';
    end else if LoadLibrary('RICHED20.DLL') <> 0 then begin
      if UnicodeEnabledOS then GlobalRichClass := 'RichEdit20W'
      else
      GlobalRichClass := 'RichEdit20A'
    end else if LoadLibrary('RICHED32.DLL') <> 0 then begin
      GlobalRichClass := 'RichEdit';
    end;
      
    if not Assigned(RichEditManager) then 
      RichEditManager := TRichEditManager;
      
    Result := GlobalRichClass <> '';
  end else
    Result:=true;
end;

procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
begin
  if length(s) < ChrsSize then ChrsSize := length(s);
  if length(s) > 0 then Move(s[1], Chrs[0], ChrsSize);
end;

function FontStylesToEffects(Styles: TFontStyles): LongWord;
begin
  Result := 0;
  if fsBold in Styles then Result := Result or CFE_BOLD;
  if fsItalic in Styles then Result := Result or CFE_ITALIC;
  if fsStrikeOut in Styles then Result := Result or CFE_STRIKEOUT;
  if fsUnderline in Styles then Result := Result or CFE_UNDERLINE;
end;

function EffectsToFontStyles(Effects: LongWord): TFontStyles;
begin
  Result := [];
  if Effects and CFE_BOLD > 0 then Include(Result, fsBold);
  if Effects and CFE_ITALIC > 0 then Include(Result, fsItalic);
  if Effects and CFE_STRIKEOUT > 0 then Include(Result, fsStrikeOut);
  if Effects and CFE_UNDERLINE > 0 then Include(Result, fsUnderline);
end;

function VScriptPosToEffects(vpos: TVScriptPos): LongWord;
const
  EffMask : array [TVScriptPos] of LongWord = (0, CFE_SUBSCRIPT, CFE_SUPERSCRIPT);
begin
  Result:=EffMask[vpos];
end;

function EffectsToVScriptPost(Effects: LongWord): TVScriptPos;
begin
  if Effects and CFE_SUBSCRIPT > 0 then Result:=vpSubScript
  else if Effects and CFE_SUPERSCRIPT > 0 then Result:=vpSuperScript
  else Result:=vpNormal;
end;
         
procedure CharFormatToFontParams(const fmt: TCHARFORMAT; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
end;

procedure CharFormatToFontParams(const fmt: TCHARFORMAT2; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
  if fmt.cbSize > sizeof(CHARFORMAT) then begin
    Params.HasBkClr:=(fmt.dwEffects and CFE_AUTOBACKCOLOR) = 0;
    if Params.HasBkClr then Params.BkColor:=fmt.crBackColor;
    Params.VScriptPos:=EffectsToVScriptPost(fmt.dwEffects);
  end;
end;

{ TRichEditManagerWinXP }

class procedure TRichEditManagerWinXP.SetTextUIStyle(RichEditWnd: Handle;
  const ui: TTextUIParam);
var
  st      : TSetTextEx;
  linkrtf : String;
  txt     : WideString;
  txtrtf  : String;
begin
  if RichEditWnd = 0 then Exit;

  txt := GetTextW(RichEditWnd, true);
  st.codepage := CP_UTF8;
  st.flags := ST_SELECTION;
  txtrtf := UTF8Encode(txt);
  linkrtf := Format(
    '{\rtf1{\colortbl ;\red0\green0\blue238;}{\field \ul \cf1 {\*\fldinst{ HYPERLINK "%s"}}{\fldrslt{%s}\ul0 \cf0}}}',
    [ui.linkref, txtrtf]
  );
  SendMessage(RichEditWnd, EM_SETTEXTEX, WPARAM(@st), LParam(@linkrtf[1]));
end;

class function TRichEditManagerWinXP.LinkNotifyToInfo(RichEditWnd: Handle;
  const LinkNotify: TENLINK; var LinkInfo: TLinkMouseInfo): Boolean;
var
  w   : WideString;
  tr  : RichEdit.TEXTRANGEW;
  res : LResult;
begin
  FillChar(tr, sizeof(tr),0);
  tr.chrg.cpMin:=LinkNotify.chrg.cpMin;
  tr.chrg.cpMax:=LinkNotify.chrg.cpMax;
  setLength(w, tr.chrg.cpMax-tr.chrg.cpMin+1);
  tr.lpstrText:=@w[1];
  res := Windows.SendMessage(RichEditWnd, EM_GETTEXTRANGE, 0, Windows.lParam(@tr));
  LinkInfo.LinkRef := UTF8Encode( Copy(w, 1, res) );
  Result:=true;
end;

{ TRichEditManager }

class function TRichEditManager.SetEventMask(RichEditWnd: Handle; eventmask: integer): Integer;
begin
  Result := SendMessage(RichEditWnd, EM_GETEVENTMASK, 0, 0);
  SendMessage(RichEditWnd, EM_SETEVENTMASK, 0, eventmask);
end;

class function TRichEditManager.GetTextLength(RichEditWnd: Handle): Integer;
var
  textlen : TGETTEXTEX;
begin
  FillChar(textlen, sizeof(textlen), 0);
  textlen.flags := GTL_NUMCHARS or GTL_PRECISE;
  textlen.codepage := CP_UNICODE;
  Result := SendMessage(RichEditWnd, EM_GETTEXTLENGTHEX, WPARAM(@textlen), 0);
end;

class function TRichEditManager.SetDefaultTextStyle(RichEditWnd: Handle;
  Params: TIntFontParams): Boolean;
var
  w : WPARAM;
  fmt : TCHARFORMAT2;
begin
  if RichEditWnd = 0 then begin
    Result := false;
    Exit;
  end;

  w := SCF_DEFAULT;

  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  fmt.dwMask := fmt.dwMask or CFM_COLOR;
  fmt.crTextColor := Params.Color;

  fmt.dwMask := fmt.dwMask or CFM_FACE;
  // keep last char for Null-termination?
  CopyStringToCharArray(Params.Name, fmt.szFaceName, LF_FACESIZE-1);

  fmt.dwMask := fmt.dwMask or CFM_SIZE;
  fmt.yHeight := Params.Size * TwipsInFontSize;

  fmt.dwMask := fmt.dwMask or CFM_EFFECTS or CFM_SUBSCRIPT or CFM_SUPERSCRIPT;
  fmt.dwEffects := FontStylesToEffects(Params.Style) or VScriptPosToEffects(Params.VScriptPos);

  Result := SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt))>0;
end;

class function TRichEditManager.SetSelectedTextStyle(RichEditWnd: Handle; 
  Params: TIntFontParams; useMask: Boolean; AModifyMask: TTextModifyMask): Boolean;
var
  w : WPARAM;
  fmt : TCHARFORMAT2;
const
  CFM_STYLESONLY = CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT or CFM_SUBSCRIPT or CFM_SUPERSCRIPT;
begin
  if RichEditWnd = 0 then begin
    Result := false;
    Exit;
  end;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  if not useMask or (tmm_Color in AModifyMask) then begin
    fmt.dwMask := fmt.dwMask or CFM_COLOR;
    fmt.crTextColor := Params.Color;
  end;

  if not useMask or (tmm_Name in AModifyMask) then begin
    fmt.dwMask := fmt.dwMask or CFM_FACE;
    // keep last char for Null-termination?
    CopyStringToCharArray(Params.Name, fmt.szFaceName, LF_FACESIZE-1);
  end;

  if not useMask or (tmm_Size in AModifyMask) then begin
    fmt.dwMask := fmt.dwMask or CFM_SIZE;
    fmt.yHeight := Params.Size * TwipsInFontSize;
  end;

  if not useMask or (tmm_Styles in AModifyMask) then begin
    fmt.dwMask := fmt.dwMask or CFM_STYLESONLY;
    fmt.dwEffects := FontStylesToEffects(Params.Style) or VScriptPosToEffects(Params.VScriptPos);
  end;

  if not useMask or (tmm_BackColor in AModifyMask) then begin
    if Params.HasBkClr then begin
      fmt.dwMask := fmt.dwMask or CFM_BACKCOLOR;
      fmt.crBackColor := Params.BkColor;
    end else begin
      fmt.dwMask := fmt.dwMask or CFM_BACKCOLOR;
      fmt.dwEffects := fmt.dwEffects or CFE_AUTOBACKCOLOR;
    end;
  end;

  Result := SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt))>0;
end;

class function TRichEditManager.GetSelectedTextStyle(RichEditWnd: Handle;  
  var Params: TIntFontParams): Boolean; 
var
  w     : WPARAM;
  fmt   : TCHARFORMAT2;
  
begin
  Result := false;
  if RichEditWnd = 0 then Exit;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  fmt.dwMask := CFM_RICHMEMO_ATTRS;

  SendMessage(RichEditWnd, EM_GETCHARFORMAT, w, PtrInt(@fmt));
  
  CharFormatToFontParams(fmt, Params);
  Result := true;  
end;

class procedure TRichEditManager.SetTextUIStyle(RichEditWnd: Handle; const ui: TTextUIParam);
var
  w   : WPARAM;
  fmt : TCHARFORMAT2;
{  st  : TSetTextEx;
  linkrtf : String;
  txt     : WideString;
  txtrtf  : String;}
begin
  if RichEditWnd = 0 then Exit;

  w := SCF_SELECTION;

  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  fmt.dwMask := CFM_LINK;
(*    txt := GetTextW(RichEditWnd, true);
    st.codepage:=CP_ACP;
    st.flags:=ST_SELECTION;
    txtrtf:=txt;
    writeln('txtrtf = ', txtrtf);
    linkrtf:=Format('{\rtf1{\field{\*\fldinst{ HYPERLINK "%s"}}{\fldrslt{%s}}}}',
      [ui.linkref, txtrtf]);
    SendMessage(RichEditWnd, EM_SETTEXTEX, WPARAM(@st), LParam(@linkrtf[1])); *)

  if uiLink in ui.features then fmt.dwEffects := fmt.dwEffects or CFE_LINK;

  SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt));
end;

class function TRichEditManager.GetTextUIStyle(RichEditWnd: Handle; var ui: TTextUIParam): Boolean;
var
  w   : WPARAM;
  fmt : TCHARFORMAT2;
begin
  Result:=false;
  if RichEditWnd = 0 then Exit;

  w := SCF_SELECTION;

  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  fmt.dwMask := CFM_LINK;

  SendMessage(RichEditWnd, EM_GETCHARFORMAT, w, PtrInt(@fmt));
  InitTextUIParams(ui);
  if fmt.dwEffects and CFE_LINK > 0 then
    Include(ui.features, uiLink);
  Result:=true;
end;

type
  richedit_gettextlengthex = packed record
    flags     : DWORD;
    codepage  : LongWord;
  end;
  Tgettextlengthex = richedit_gettextlengthex;

class function TRichEditManager.GetStyleRange(RichEditWnd: Handle; TextStart: Integer; 
  var RangeStart, RangeLen: Integer): Boolean; 
var
  len     : integer;
  fmt     : TCHARFORMAT;
  textlen : Tgettextlengthex;
  sel     : TCHARRANGE;
  d       : Integer;
  last    : Integer;
  initMask : DWORD;
const
  ALL_MASK = CFM_RICHMEMO_ATTRS;
begin
  Result := false;
  if (RichEditWnd = 0) then Exit;
  
  FillChar(textlen, sizeof(textlen), 0);
  textlen.flags := GTL_NUMCHARS or GTL_PRECISE;
  textlen.codepage := CP_UNICODE;
  len := SendMessage(RichEditWnd, EM_GETTEXTLENGTHEX, WPARAM(@textlen), 0);
  Result := TextStart < len;
  if not Result then Exit;
   
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  sel.cpMin := TextStart;
  sel.cpMax := TextStart;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  initMask := fmt.dwMask and ALL_MASK;
  
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);

  sel.cpMin := TextStart;
  sel.cpMax := len+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  fmt.dwMask:=fmt.dwMask and ALL_MASK;

  if fmt.dwMask <> initMask then begin
    d := (len - sel.cpMin);
    while d > 1 do begin
      d := d div 2;
      if fmt.dwMask = initMask then
        sel.cpMax := sel.cpMax + d        
      else
        sel.cpMax := sel.cpMax - d;
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      fmt.dwMask:=fmt.dwMask and ALL_MASK;
    end;
    if fmt.dwMask = initMask then begin
      while (sel.cpMax <= len) and (fmt.dwMask = initMask) do begin
        inc(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
        fmt.dwMask:=fmt.dwMask and ALL_MASK;
      end;
    end else begin
      while (sel.cpMax > sel.cpMin) and (fmt.dwMask <> initMask) do begin
        dec(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
        fmt.dwMask:=fmt.dwMask and ALL_MASK;
      end;
      inc(sel.cpMax);
    end;
  end;
  last := sel.cpMax;  
  
  sel.cpMin := 0;
  sel.cpMax := TextStart+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  fmt.dwMask:=fmt.dwMask and ALL_MASK;
  if fmt.dwMask <> initMask then begin
    d := TextStart;
    while d > 1 do begin
      d := d div 2;
      if fmt.dwMask = initMask then
        dec(sel.cpMin,d)
      else
        inc(sel.cpMin,d);
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      fmt.dwMask:=fmt.dwMask and ALL_MASK;
    end;
    if (fmt.dwMask = initMask) then begin
      while (sel.cpMin > 0) and (fmt.dwMask = initMask) do begin
        dec(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
        fmt.dwMask:=fmt.dwMask and ALL_MASK;
      end;
      if (fmt.dwMask = initMask) then inc(sel.cpMin);
    end else begin
      while (sel.cpMin < TextStart) and (fmt.dwMask <> initMask) do begin
        inc(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
        fmt.dwMask:=fmt.dwMask and ALL_MASK;
      end;
    end;
  end;  
 
  RangeStart := sel.cpMin;
  RangeLen := last - sel.cpMin - 1;

  Result := true;  
end;

class procedure TRichEditManager.GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMax := 0;
  Range.cpMin := 0;
  SendMessage(RichEditWnd, EM_EXGETSEL, 0, PtrInt(@Range));
  TextStart := Range.cpMin;
  TextLen := Range.cpMax-Range.cpMin;
end;

class procedure TRichEditManager.SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMin := TextStart;
  Range.cpMax := TextStart + TextLen;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, PtrInt(@Range));
end;

class procedure TRichEditManager.GetSelRange(RichEditWnd: Handle; var sr: TCHARRANGE);
var
  st: Integer;
begin
  sr.cpMax := 0;
  sr.cpMin := 0;
  st:=0;
  SendMessage(RichEditWnd, EM_EXGETSEL, 0, PtrInt(@sr));
  // EM_EXGETSEL - always returns min and max, in the math order
  // (where math is lower, than max)
  // This, however, doesn't match the seletion direction.
  // Selection direction is done by either mouse (right to left) and (left to right)
  // or by holding SHIFT key and moving left to right.
  // EM_EXSETSEL - repsects the specified sr.cpMax and sr.cpMin order

  // Resetting the selection.
  // This is a bit hacky, BUT the selection would be reset
  // towards the direction of the selection
  SendMessage(RichEditWnd, EM_SETSEL, -1, 0);
  SendMessage(RichEditWnd, EM_GETSEL, WPARAM(@st), 0);

  if st=sr.cpMin then begin // right-to-left selection
    sr.cpMin:=sr.cpMax;
    sr.cpMax:=st;
  end;
end;

class procedure TRichEditManager.SetSelRange(RichEditWnd: Handle; const sr: TCHARRANGE);
begin
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, PtrInt(@sr));
end;

class procedure TRichEditManager.SetHideSelection(RichEditWnd: Handle; AValue: Boolean);
var
  style  : LResult;
begin
  // res-setting options might RichEdit style. Must restore it, after option is changed
  style := GetWindowLong(RichEditWnd, GWL_STYLE);
  if AValue then
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_AND, not ECO_NOHIDESEL)
  else
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_OR, ECO_NOHIDESEL);
  SetWindowLong(RichEditWnd, GWL_STYLE, style);
end;

type
  TEditStream_ = packed record
    dwCookie    : PDWORD;
    dwError     : DWORD;
    pfnCallback : EDITSTREAMCALLBACK;
  end;
  
{$IF FPC_FULLVERSION>=30202}
function RTFLoadCallback(dwCookie:DWORD_PTR; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ELSE}
function RTFLoadCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ENDIF}
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ASrc);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFLoadCallback;
  SendMessage(RichEditWnd, EM_STREAMIN, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

{$IF FPC_FULLVERSION>=30202}
function RTFSaveCallback(dwCookie:DWORD_PTR; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ELSE}
function RTFSaveCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ENDIF}
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Write(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.SaveRichText(RichEditWnd: Handle; ADst: TStream; extraflags: Integer): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ADst);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFSaveCallback;
  SendMessage(RichEditWnd, EM_STREAMOUT, SF_RTF or extraFlags, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

class procedure TRichEditManager.SetText(RichEditWnd:Handle;
  const Text: WideString; TextStart, ReplaceLength:Integer);
var
  AnsiText : AnsiString;
  txt      : PChar;
  sr       : TCHARRANGE;
const
  UNDO_FLAG=1; // can be undone
begin
  GetSelRange(RichEditWnd, sr);
  SetSelection(RichEditWnd, TextStart, ReplaceLength);

  txt:=nil;
  if UnicodeEnabledOS then begin
    if Text<>'' then txt:=@Text[1];
    SendMessageW(RichEditWnd, EM_REPLACESEL, UNDO_FLAG, LPARAM(txt));
  end else begin
    AnsiText:=Text;
    if AnsiText<>'' then txt:=@AnsiText[1];
    SendMessageA(RichEditWnd, EM_REPLACESEL, UNDO_FLAG, LPARAM(txt));
  end;

  SetSelRange(RichEditWnd, sr);
end;

class function TRichEditManager.GetTextW(RichEditWnd: Handle;
  inSelection: Boolean): WideString;
var
  t   : GETTEXTEX;
  res : Integer;
  w   : WideString;
  st  : Integer;
begin
  if inSelection then
    GetSelection(RichEditWnd, st, res)
  else
    res:=GetTextLength(RichEditWnd);

  if res>0 then begin
    SetLength(w, res);
    FillChar(t, sizeof(t), 0);
    t.cb:=(length(w)+1)*sizeof(WideChar);
    t.flags:=GT_DEFAULT;
    if inSelection then t.flags:=t.flags or GT_SELECTION;
    t.codepage:=CP_WINUNICODE;
    res:=SendMessageW(RichEditWnd, EM_GETTEXTEX, WPARAM(@t), LPARAM(@w[1]));
    Result:=w;
  end else
    Result:='';
end;

class function TRichEditManager.GetTextA(RichEditWnd: Handle;
  inSelection: Boolean): AnsiString;
var
  t   : GETTEXTEX;
  res : Integer;
  s   : AnsiString;
  st  : Integer;
begin
  if inSelection then
    GetSelection(RichEditWnd, st, res)
  else
    res:=GetTextLength(RichEditWnd);

  if res>0 then begin
    SetLength(s, res);
    FillChar(t, sizeof(t), 0);
    t.cb:=length(s)+1;
    t.flags:=GT_DEFAULT;
    t.codepage:=CP_ACP;
    res:=SendMessageA(RichEditWnd, EM_GETTEXTEX, WPARAM(@t), LPARAM(@s[1]));
    Result:=s;
  end else
    Result:='';
end;

class function TRichEditManager.GetTextUtf8(RichEditWnd: Handle;
  inSelection: Boolean): string;
begin
  if UnicodeEnabledOS then
    Result:=UTF8Encode(GetTextW(RichEditWnd, inSelection))
  else
    Result:=AnsiToUtf8(GetTextA(RichEditWnd, inSelection));
end;

class procedure TRichEditManager.GetPara2(RichEditWnd: Handle; TextStart: Integer;
  var para: PARAFORMAT2);
var
  sr : TCHARRANGE;
begin
  GetSelRange(RichEditWnd, sr);

  SetSelection(RichEditWnd, TextStart, 0);

  FillChar(para, sizeof(para), 0);
  para.cbSize:=sizeof(para);
  SendMessagea(RichEditWnd, EM_GETPARAFORMAT, 0, LPARAM(@para));

  SetSelRange(RichEditWnd, sr);
end;

class procedure TRichEditManager.SetPara2(RichEditWnd: Handle;
  TextStart, TextLen: Integer; const para: PARAFORMAT2);
var
  sr : TCHARRANGE;
begin
  GetSelRange(RichEditWnd, sr);

  SetSelection(RichEditWnd, TextStart, TextLen);
  SendMessagea(RichEditWnd, EM_SETPARAFORMAT, 0, LPARAM(@para));

  SetSelRange(RichEditWnd, sr);
end;

class function TRichEditManager.Find(RichEditWnd: THandle; const ANiddle: WideString; const ASearch: TIntSearchOpt): Integer; overload;
var
  l : integer;
begin
  Result:=Find(RichEDitWnd, ANiddle, ASearch, l);
end;

class function TRichEditManager.Find(RichEditWnd: THandle;
  const ANiddle: WideString; const ASearch: TIntSearchOpt; var TextLen: Integer): Integer;
var
  fw: TFINDTEXTEXW;
  fa: TFINDTEXTEXA;
  opt: WParam;
  txt: string;
  mn, mx : Integer;
begin
  if ANiddle='' then begin
    Result:=-1;
    Exit;
  end;
  opt:=0;
  if not (soBackward in ASearch.Options) then opt:=FR_DOWN; // if not set, then search is backward
  if soMatchCase in ASearch.Options then opt := opt or FR_MATCHCASE;
  if soWholeWord in ASearch.Options then opt := opt or FR_WHOLEWORD;
  mn := ASearch.start;
  mx := 0;
  if soBackward in ASearch.Options then begin
    if ASearch.len<0 then mx := 0
    else begin
      mx := ASearch.start-ASearch.len;
      if mx < 0 then mx:=0;
    end;
  end else begin
    if ASearch.len<0 then fw.chrg.cpMax := -1
    else begin
      mx := ASearch.start+ASearch.len;
      if mx < 0 then mx:=-1;
    end;
  end;

  if UnicodeEnabledOS then begin
    fw.chrg.cpMin := mn;
    fw.chrg.cpMax := mx;
    fw.lpstrText := PWideChar(@ANiddle[1]);
    Result := SendMessage(RichEditWnd, EM_FINDTEXTEXW, opt, LParam(@fw));
    if Result>=0 then TextLen:=fw.chrgText.cpMax-fw.chrgText.cpMin;
  end else begin
    fa.chrg.cpMin := mn;
    fa.chrg.cpMax := mx;
    txt:=ANiddle;
    fa.lpstrText := PAnsiChar(@txt[1]);
    Result := SendMessage(RichEditWnd, EM_FINDTEXTEX, opt, LParam(@fa));
    if Result>=0 then TextLen:=fa.chrgText.cpMax-fa.chrgText.cpMin;
  end;
end;

class procedure TRichEditManager.GetParaRange(RichEditWnd: Handle; TextStart: integer;
  var para: TParaRange);
var
  line: Integer;
  //txtlen: Integer;
  st: Integer;
  ln: Integer;
  toend: Integer;
  tost: Integer;
  buf : string[16];
  rng : TTEXTRANGEA;
  res : Integer;
begin
  //txtlen:=GetTextLength(RichEditWnd);
  // lines are NOT paragraphs, but wordwrapped lines
  line:=SendMessage(RichEditWnd, EM_EXLINEFROMCHAR, 0, TextStart);
  st:=SendMessage(RichEditWnd, EM_LINEINDEX, line, 0);
  tost:=st;
  toend:=0;

  while tost>0 do begin
    rng.lpstrText:=@buf[1];
    rng.chrg.cpMin:=tost-1;
    rng.chrg.cpMax:=tost;
    buf[1]:=#0;
    res:=SendMessageA(RichEditWnd, EM_GETTEXTRANGE, 0, LPARAM(@rng));
    if (buf[1]=HardBreak) then
      Break // found the beggining of the paragraph
    else begin
      line:=SendMessage(RichEditWnd, EM_EXLINEFROMCHAR, 0, tost-2); // getting the line before the linebreak
      tost:=SendMessage(RichEditWnd, EM_LINEINDEX, line, 0);
      inc(toend, SendMessage(RichEditWnd, EM_LINELENGTH, line, 0));
    end;
  end;

  repeat
    ln:=SendMessage(RichEditWnd, EM_LINELENGTH, st, 0);
    inc(toend, ln);
    inc(st, ln);
    rng.lpstrText:=@buf[1];
    rng.chrg.cpMin:=st;
    rng.chrg.cpMax:=st+1;
    buf[1]:=#0;
    res:=SendMessage(RichEditWnd, EM_GETTEXTRANGE, 0, LPARAM(@rng));
  until (res=0) or (buf[1] = HardBreak);

  para.start:=tost;
  para.lengthNoBr:=toend;
  if res>0 then inc(toend); // there's a line break character - add it to the range
  para.length:=toend;
end;

class procedure TRichEditManager.GetScroll(RichEditWnd: Handle; out pt: TPoint);
begin
  SendMessage(RichEditWnd, EM_GETSCROLLPOS, 0, LPARAM(@pt));
end;

class procedure TRichEditManager.SetScroll(RichEditWnd: Handle; const pt: TPoint);
begin
  SendMessage(RichEditWnd, EM_SETSCROLLPOS, 0, LPARAM(@pt));
end;

class function TRichEditManager.LinkNotifyToInfo(RichEditWnd: Handle;
  const LinkNotify: TENLINK; var LinkInfo: TLinkMouseInfo): Boolean;
var
  gt : GETTEXTEX;
  l  : integer;
  olds, oldl : integer;
  st : TStringStream;
  evmsk : Integer;
  hp : string;
  hpi : integer;
  hps : integer;
begin
  evmsk := SetEventMask(RichEditWnd, 0);
  try
    GetSelection(RichEditWnd, olds, oldl);
    try
      l := LinkNotify.chrg.cpMax - LinkNotify.chrg.cpMin;
      SetSelection(RichEditWnd, LinkNotify.chrg.cpMin, l);
      st := TStringStream.Create('');
      hp := '';
      try
        SaveRichText(RichEditWnd, st, SFF_SELECTION);
        hp := st.DataString;
      finally
        st.Free;
      end;
      hpi := Pos('HYPERLINK', hp);
      if (hpi>0) then begin
        inc(hpi, length('HYPERLINK'));
        while (hpi <= length(hp)) and not (hp[hpi] in ['"','}']) do inc(hpi);
        if (hpi <= length(hp)) and (hp[hpi]='"') then begin
          inc(hpi);
          hps:=hpi;
          while (hpi <= length(hp)) and (hp[hpi] <> '"') do
            inc(hpi);
          //todo: convert RTF chars into UTF8
          LinkInfo.LinkRef := Copy(hp, hps, hpi-hps);
        end;
        Result := true;
      end else
        Result :=TRichEditManagerWinXP.LinkNotifyToInfo(RichEditWnd, LinkNotify, LinkInfo);
    finally
      SetSelection(RichEditWnd, olds, oldl);
    end;
  finally
    SetEventMask(RichEditWnd, evmsk);
  end;
end;

function WinInsertImageFromFile (const ARichMemo: TCustomRichMemo; APos: Integer;
   const FileNameUTF8: string;
   const AImgSize: TSize): Boolean;
var
  hnd : THandle;
  rch : IRichEditOle;
  Fmt : FORMATETC;
  FN : WideString;
  LockBytes: ILockBytes;
  ClientSite: IOleClientSite;
  Storage: IStorage;
  Image: IOleObject;
  Obj: TREOBJECT;

  sl, ss: Integer;
const
  PointSize     = 72.0;
  RtfSizeToInch = 2.54 * 1000.0;
  SizeFactor    = 1 / PointSize * RtfSizeToInch;
begin
  Result:=false;
  if not Assigned(ARichMemo) then Exit;
  if not ARichMemo.HandleAllocated then begin
    ARichMemo.HandleNeeded;
    if not ARichMemo.HandleAllocated then Exit;
  end;
  if (FileNameUTF8 ='') then Exit;

  ss:=ARichMemo.SelStart;
  sl:=ARichMemo.SelLength;
  try
    hnd:= THandle(ARichMemo.Handle);
    SendMessage(hnd, EM_GETOLEINTERFACE, 0, LPARAM(@rch));

    FillChar(Fmt, sizeoF(Fmt), 0);
    Fmt.dwAspect:=DVASPECT_CONTENT;
    Fmt.lindex:=-1;

    CreateILockBytesOnHGlobal(0, True, LockBytes);
    StgCreateDocfileOnILockBytes(LockBytes, STGM_SHARE_EXCLUSIVE or STGM_CREATE or STGM_READWRITE, 0, Storage);
    rch.GetClientSite(ClientSite);

    FN := UTF8Decode( FileNameUTF8 );
    OleCreateFromFile(CLSID_NULL, @FN[1], IOleObject
       , OLERENDER_DRAW, @Fmt, ClientSite, Storage, Image);
    OleSetContainedObject(Image, True);

    FillChar(Obj, sizeof(Obj),0);
    Obj.cbStruct := SizeOf(Obj);
    Obj.cp := REO_CP_SELECTION;
    Image.GetUserClassID(Obj.clsid);
    Obj.poleobj := Image;
    Obj.pstg := Storage;
    Obj.polesite := ClientSite;
    Obj.dvaspect := DVASPECT_CONTENT;
    if (AImgSize.cx<>0) or (AImgSize.cy<>0) then begin
      //http://msdn.microsoft.com/en-us/library/windows/desktop/bb787946%28v=vs.85%29.aspx
      //The size of the object. The unit of measure is 0.01 millimeters, which is a HIMETRIC measurement.
      Obj.sizel.cx:=round(AImgSize.cx * SizeFactor);
      Obj.sizel.cy:=round(AImgSize.cy * SizeFactor);
    end;

    ARichMemo.SelStart:=APos;
    ARichMemo.SelLength:=0;
    Result:= Succeeded(rch.InsertObject(obj));
  finally
    ARichMemo.SelStart:=ss;
    ARichMemo.SelLength:=sl;
  end;
end;

initialization
  InsertImageFromFile := @WinInsertImageFromFile;
  RichEditManager := TRichEditManager;

end.                                            

