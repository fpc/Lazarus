unit lr_e_gen;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,LR_E_TXT,
  LCLType,LCLIntf,LR_Class, LCLProc;

type

  { TfrDBGExport }

  TfrDBGExport = class(TComponent)
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TfrDBGExportFilter }

  TfrDBGExportFilter = class(TfrExportFilter)
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnText(X, Y: Integer; const Text: String; View: TfrView); override;
  end;

implementation

{ TfrDBGExport }

constructor TfrDBGExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  frRegisterExportFilter(TfrDBGExportFilter, 'Debug Export Filter (*.dbg)', '*.dbg');
end;

{ TfrDBGExportFilter }

constructor TfrDBGExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  WriteLn('DebugExportFilter: Created');
end;

destructor TfrDBGExportFilter.Destroy;
begin
  inherited Destroy;
  WriteLn('DebugExportFilter: Destroyed');
end;

procedure TfrDBGExportFilter.OnBeginPage;
begin
  WriteLn(' DebugExportFilter: OnBeginPage');
end;

procedure TfrDBGExportFilter.OnEndPage;
begin
  WriteLn(' DebugExportFilter: OnEndPage');
end;

function TypToStr(Typ:Integer):string;
begin
  case Typ of
    gtMemo                   : result := 'gtMemo';
    gtPicture                : result := 'gtPicture';
    gtBand                   : result := 'gtBand';
    gtSubReport              : result := 'gtSubReport';
    gtLine                   : result := 'gtLine';
    gtAddIn                  : result := 'gtAddIn';
    else                       result := 'gt?????????';
  end;
end;
function BandToStr(Bt: TfrBandType): string;
begin
  case bt of
    btReportTitle       : result := 'btReportTitle';
    btReportSummary     : result := 'btReportSummary';
    btPageHeader        : result := 'btPageHeader';
    btPageFooter        : result := 'btPageFooter';
    btMasterHeader      : result := 'btMasterHeader';
    btMasterData        : result := 'btMasterData';
    btMasterFooter      : result := 'btMasterFooter';
    btDetailHeader      : result := 'btDetailHeader';
    btDetailData        : result := 'btDetailData';
    btDetailFooter      : result := 'btDetailFooter';
    btSubDetailHeader   : result := 'btSubDetailHeader';
    btSubDetailData     : result := 'btSubDetailData';
    btSubDetailFooter   : result := 'btSubDetailFooter';
    btOverlay           : result := 'btOverlay';
    btColumnHeader      : result := 'btColumnHeader';
    btColumnFooter      : result := 'btColumnFooter';
    btGroupHeader       : result := 'btGroupHeader';
    btGroupFooter       : result := 'btGroupFooter';
    btCrossHeader       : result := 'btCrossHeader';
    btCrossData         : result := 'btCrossData';
    btCrossFooter       : result := 'btCrossFooter';
    btNone              : result := 'btNone';
  else
    result := 'Band?????';
  end;

end;

procedure TfrDBGExportFilter.OnData(x, y: Integer; View: TfrView);
begin
  if View.Flags and flStartRecord <>0 then WriteLn;
  Write('   OnData [');
  if View.Flags and flStartRecord <>0 then Write(' StartRecord');
  if View.Flags and flEndRecord <>0 then Write(' EndRecord');
  Write('] X=',x,' Y=',Y,' View=',View.Name,':',View.ClassName,' Typ=',TypToStr(View.Typ));
  Write(' ParentBand=',BandToStr(View.ParentBandType));
  Writeln(' Memo=',dbgstr(View.Memo.Text));
end;

procedure TfrDBGExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
begin
  writeLn('     OnText X=',X,' Y=',Y,' View=',View.Name,':',View.ClassName,' text=',dbgstr(Text));
end;

end.

