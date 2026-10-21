{
 Win32RichMemoOle.pas

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
unit Win32RichMemoOle;

interface

{$if FPC_FULLVERSION < 30200}
{$undef MEDVAR}
{$else}
{$define MEDVAR}
{$endif}

uses
  Windows, ActiveX, ComObj, Win32RichMemoProc;


{.$define oledebug}

type

  { TCustomObject }

  TCustomObject = class(TInterfacedObject, IOleObject)
  protected
    fSink : IAdviseSink;
    function SetClientSite(const clientSite: IOleClientSite): HResult;StdCall;
    function GetClientSite(out clientSite: IOleClientSite): HResult;StdCall;
    function SetHostNames(szContainerApp: POleStr; szContainerObj: POleStr): HResult;StdCall;
    function Close(dwSaveOption: DWORD): HResult;StdCall;
    function SetMoniker(dwWhichMoniker: DWORD; const mk: IMoniker): HResult;StdCall;
    function GetMoniker(dwAssign: DWORD; dwWhichMoniker: DWORD; out mk: IMoniker): HResult;StdCall;
    function InitFromData(const dataObject: IDataObject; fCreation: BOOL; dwReserved: DWORD): HResult;StdCall;
    function GetClipboardData({%H-}dwReserved: DWORD; out {%H-}dataObject: IDataObject): HResult;StdCall;
    function DoVerb(iVerb: LONG; msg: PMsg; const activeSite: IOleClientSite; lindex: LONG; hwndParent: HWND; const posRect: TRect): HResult;StdCall;
    function EnumVerbs(out {%H-}enumOleVerb: IEnumOleVerb): HResult;StdCall;
    function Update: HResult;StdCall;
    function IsUpToDate: HResult;StdCall;
    function GetUserClassID(out clsid: TCLSID): HResult;StdCall;
    function GetUserType(dwFormOfType: DWORD; out pszUserType: POleStr): HResult;StdCall;
    function SetExtent(dwDrawAspect: DWORD; const size: TPoint): HResult;StdCall;
    function GetExtent(dwDrawAspect: DWORD; out size: TPoint): HResult;StdCall;
    function Advise(const advSink: IAdviseSink; out dwConnection: Longint): HResult;StdCall;
    function Unadvise(dwConnection: DWORD): HResult;StdCall;
    function EnumAdvise(out aenumAdvise: IEnumStatData): HResult;StdCall;
    function GetMiscStatus(dwAspect: DWORD; out dwStatus: DWORD): HResult;StdCall;
    function SetColorScheme(const logpal: TLogPalette): HResult;StdCall;
  end;

  { TCustomDataObject }

  TCustomDataObject = class(TCustomObject, IOleObject, IDataObject)
    Function GetData(Const formatetcIn : FORMATETC;Out medium : STGMEDIUM):HRESULT; STDCALL;
    Function GetDataHere(CONST pformatetc : FormatETC; Out medium : STGMEDIUM):HRESULT; STDCALL;
    Function QueryGetData(const pformatetc : FORMATETC):HRESULT; STDCALL;
    Function GetCanonicalFormatEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult; STDCALl;
    Function SetData (Const pformatetc : FORMATETC;
      {$ifdef MEDVAR}var{$ELSE}const{$ENDIF} medium:STGMEDIUM;
      FRelease : BOOL):HRESULT; StdCall;
    Function EnumFormatEtc(dwDirection : DWord; OUT enumformatetcpara : IENUMFORMATETC):HRESULT; StdCall;
    Function DAdvise(const formatetc : FORMATETC;advf :DWORD; CONST AdvSink : IAdviseSink;OUT dwConnection:DWORD):HRESULT;StdCall;
    Function DUnadvise(dwconnection :DWord) :HRESULT;StdCall;
    Function EnumDAdvise(Out aenumAdvise : IEnumStatData):HResult;StdCall;
  end;

  { TCustomDataViewObject }

  TCustomDataViewObject = class(TCustomDataObject, IOleObject, IDataObject, IViewObject)
    function Draw(dwDrawAspect:DWord;LIndex:Long;pvaspect:pointer;ptd:PDVTARGETDEVICE;hdcTargetDev:HDC; hdcDraw:HDC;lprcBounds:PRECTL;lprcWBounds:PRECTL;pfncontinue:TContinueCallback;dwcontinue:ULONG_PTR):HResult; stdcall;
    function GetColorSet(wDrawAspect:DWord;LIndex:Long;pvaspect:pointer;ptd:PDVTARGETDEVICE;hdcTargetDev:HDC;var ppcolorset:PLogPalette):HREsult; stdcall;
    function Freeze(dwDrawAspect:DWord;LIndex:Long;pvaspect:pointer;pdwfreeze:pdword):HResult;stdcall;
    function Unfreeze(dwfreeze:dword):HResult; stdcall;
    function SetAdvise(aspects:DWORD;advf:DWORD;padvSink:IAdviseSink):HRESULT;stdcall;
    function Getadvise(paspects:pdword;padvf:pdword;out ppadvsink: IADviseSink):HRESULT;stdcall;
  end;

implementation

{ TCustomObject }

function TCustomObject.SetClientSite(const clientSite: IOleClientSite
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.SetClientSide');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetClientSite(out clientSite: IOleClientSite): HResult;
  StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetClientSite');{$endif}
  Result:=S_OK;
end;

function TCustomObject.SetHostNames(szContainerApp: POleStr;
  szContainerObj: POleStr): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.SetHostNames');{$endif}
  Result:=S_OK;
end;

function TCustomObject.Close(dwSaveOption: DWORD): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.Close');{$endif}
  Result:=S_OK;
end;

function TCustomObject.SetMoniker(dwWhichMoniker: DWORD; const mk: IMoniker
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.SetMoniker');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetMoniker(dwAssign: DWORD; dwWhichMoniker: DWORD; out
  mk: IMoniker): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetMoniker');{$endif}
  Result:=E_NOTIMPL;
end;

function TCustomObject.InitFromData(const dataObject: IDataObject;
  fCreation: BOOL; dwReserved: DWORD): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.InitFromData');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetClipboardData(dwReserved: DWORD; out
  dataObject: IDataObject): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetClipboardData');{$endif}
  Result:=E_NOTIMPL;
end;

function TCustomObject.DoVerb(iVerb: LONG; msg: PMsg;
  const activeSite: IOleClientSite; lindex: LONG; hwndParent: HWND;
  const posRect: TRect): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.DoVerb');{$endif}
  Result:=S_OK;
end;

function TCustomObject.EnumVerbs(out enumOleVerb: IEnumOleVerb): HResult;
  StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.EnumVerbs');{$endif}
  Result:=S_OK;
end;

function TCustomObject.Update: HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.Update');{$endif}
  Result:=S_OK;
end;

function TCustomObject.IsUpToDate: HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.IsUpToDate');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetUserClassID(out clsid: TCLSID): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetUserClassID');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetUserType(dwFormOfType: DWORD; out pszUserType: POleStr
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetUserType');{$endif}
  Result:=S_OK;
end;

function TCustomObject.SetExtent(dwDrawAspect: DWORD; const size: TPoint
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.SetExtent');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetExtent(dwDrawAspect: DWORD; out size: TPoint
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetExtent');{$endif}
  Result:=S_OK;
end;

function TCustomObject.Advise(const advSink: IAdviseSink; out
  dwConnection: Longint): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.Advise');{$endif}
  fSink := advSink;
  dwConnection:=1;
  Result:=S_OK;
end;

function TCustomObject.Unadvise(dwConnection: DWORD): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.Unadvise');{$endif}
  fSink:=nil;
  Result:=S_OK;
end;

function TCustomObject.EnumAdvise(out aenumAdvise: IEnumStatData): HResult;
  StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.EnumAdvise');{$endif}
  Result:=S_OK;
end;

function TCustomObject.GetMiscStatus(dwAspect: DWORD; out dwStatus: DWORD
  ): HResult; StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.GetMiscStatus');{$endif}
  Result:=S_OK;
end;

function TCustomObject.SetColorScheme(const logpal: TLogPalette): HResult;
  StdCall;
begin
  {$ifdef oledebug}writeln('IOleObject.SetColorScheme');{$endif}
  Result:=S_OK;
end;

{ TCustomDataObject }

function TCustomDataObject.GetData(const formatetcIn: FORMATETC; out
  medium: STGMEDIUM): HRESULT; STDCALL;
begin
  {$ifdef oledebug}writeln('IDataObject.getData');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.GetDataHere(const pformatetc: FormatETC; out
  medium: STGMEDIUM): HRESULT; STDCALL;
begin
  {$ifdef oledebug}writeln('IDataObject.GetDataHere');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.QueryGetData(const pformatetc: FORMATETC): HRESULT;
  STDCALL;
begin
  {$ifdef oledebug}writeln('IDataObject.QueryGetData');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.GetCanonicalFormatEtc(const pformatetcIn: FORMATETC;
  out pformatetcOut: FORMATETC): HResult; STDCALl;
begin
  {$ifdef oledebug}writeln('IDataObject.GetCanonicalFormatEtc');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.SetData(const pformatetc: FORMATETC;
  {$ifdef MEDVAR}var{$ELSE}const{$ENDIF} medium: STGMEDIUM;
  FRelease: BOOL): HRESULT; StdCall;
begin
  {$ifdef oledebug}writeln('IDataObject.SetData');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.EnumFormatEtc(dwDirection: DWord; out
  enumformatetcpara: IENUMFORMATETC): HRESULT; StdCall;
begin
  {$ifdef oledebug}writeln('IDataObject.EnumFormatEtc');{$endif}
  Result:=E_NOTIMPL;
end;

function TCustomDataObject.DAdvise(const formatetc: FORMATETC; advf: DWORD;
  const AdvSink: IAdviseSink; out dwConnection: DWORD): HRESULT; StdCall;
begin
  {$ifdef oledebug}writeln('IDataObject.DAdvise');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.DUnadvise(dwconnection: DWord): HRESULT; StdCall;
begin
  {$ifdef oledebug}writeln('IDataObject.DUnadvise');{$endif}
  Result:=S_OK;
end;

function TCustomDataObject.EnumDAdvise(out aenumAdvise: IEnumStatData): HResult;
  StdCall;
begin
  {$ifdef oledebug}writeln('IDataObject.EnumDAdvise');{$endif}
  Result:=S_OK;
end;


{ TCustomDataViewObject }

function TCustomDataViewObject.Draw(dwDrawAspect: DWord; LIndex: Long;
  pvaspect: pointer; ptd: PDVTARGETDEVICE; hdcTargetDev: HDC; hdcDraw: HDC;
  lprcBounds: PRECTL; lprcWBounds: PRECTL; pfncontinue: TContinueCallback;
  dwcontinue: ULONG_PTR): HResult; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.Draw');{$endif}
  Result:=S_OK;
end;

function TCustomDataViewObject.GetColorSet(wDrawAspect: DWord; LIndex: Long;
  pvaspect: pointer; ptd: PDVTARGETDEVICE; hdcTargetDev: HDC;
  var ppcolorset: PLogPalette): HREsult; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.GetColorSet');{$endif}
  Result:=S_OK;
end;

function TCustomDataViewObject.Freeze(dwDrawAspect: DWord; LIndex: Long;
  pvaspect: pointer; pdwfreeze: pdword): HResult; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.Freeze');{$endif}
  Result:=S_OK;
end;

function TCustomDataViewObject.Unfreeze(dwfreeze: dword): HResult; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.Unfreeze');{$endif}
  Result:=S_OK;
end;

function TCustomDataViewObject.SetAdvise(aspects: DWORD; advf: DWORD;
  padvSink: IAdviseSink): HRESULT; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.SetAdvise');{$endif}
  Result:=S_OK;
end;

function TCustomDataViewObject.Getadvise(paspects: pdword; padvf: pdword; out
  ppadvsink: IADviseSink): HRESULT; stdcall;
begin
  {$ifdef oledebug}writeln('IDataView.Getadvise');{$endif}
  Result:=S_OK;
end;

end.

