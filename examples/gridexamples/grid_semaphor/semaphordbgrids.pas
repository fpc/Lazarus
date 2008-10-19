{*****************************************************************************
                               SemaphorDBGrid.pas
                             -------------------
                             Lazarus LCL Component
                          First Release: January 2005

  Author: Salvatore Coppola - Calabria (Italy)
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

{ABSTRACT
SEMAFORO (Semaphor) in Italian Language means Traffic Lights. If Semaphor is
set to true,when TSemaphorDBGrid detect in a non Fixed Cells a string like
StringGreen or StringYellow or StringRed, it show a colored sign in the
corrispondent cells (shape choosed in SemaphorShape). It can be Case Sensitive
(SemaphorCaseSensitive). If Semaphor is false, nothing happen.

That's all
Enjoy! Salvatore
}

unit SemaphorDBGrids;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, LCLIntf, LCLType, Forms, Controls,
  Graphics, Dialogs, Grids, DBGrids;

type
  TSemaphorShape=(ssTopBar, ssBottomBar, ssLeftBar, ssRigthBar, ssTopLeftSquare,
                  ssTopRigthSquare, ssBottomLeftSquare, ssBottomRigth, ssDisk);

type
  TSemaphorDBGrid = class(TdbGrid)
  private
    { Private declarations }
    FSemaphor : boolean;
    FStringRed : string;
    FStringYellow : string;
    FStringGreen : string;
    FSemaphorShape : TSemaphorShape;
    FSemaphorCaseSensitive : boolean;
    procedure SetSemaphorShape(Value : TSemaphorShape);
  protected
    { Protected declarations }
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Semaphor : boolean read FSemaphor write FSemaphor;
    property StringRed : string read FStringRed write FStringRed;
    property StringYellow : string read FStringYellow write FStringYellow;
    property StringGreen : string read FStringGreen write FStringGreen;
    property SemaphorShape : TSemaphorShape read FSemaphorShape write SetSemaphorShape;
    property SemaphorCaseSensitive : boolean read FSemaphorCaseSensitive write FSemaphorCaseSensitive;
  end;

procedure Register;

implementation

procedure TSemaphorDBGrid.SetSemaphorShape(Value : TSemaphorShape);
begin
  FSemaphorShape:=Value;
  invalidate
end;

procedure TSemaphorDBGrid.DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState);
const dr=4;
var Rect:TRect;
begin
  inherited DrawCell(aCol,aRow,aRect,aState);
  if not Semaphor then
    exit;
  Rect:=CellRect(aCol,aRow);
  case SemaphorShape of
    ssTopBar: Rect.Bottom:=Rect.Top+dr-1;
    ssBottomBar:Rect.Top:=Rect.Bottom-dr;
    ssLeftBar:Rect.Right:=rect.Left+dr-1;
    ssRigthBar:Rect.Left:=rect.Right-dr;
    ssTopLeftSquare:begin
      Rect.Bottom:=Rect.Top+dr;
      Rect.Right:=Rect.Left+dr;
    end;
    ssTopRigthSquare:begin
      Rect.Bottom:=Rect.Top+dr;
      Rect.Left:=Rect.Right-dr-1;
    end;
    ssBottomLeftSquare:begin
      Rect.Top:=Rect.Bottom-dr-1;
      Rect.Right:=Rect.Left+dr;
    end;
    ssBottomRigth:begin
      Rect.Top:=Rect.Bottom-dr-1;
      Rect.Left:=Rect.Right-dr-1;
    end;
    ssDisk:begin
      Rect.Bottom:=Rect.Top+2*dr-1;
      Rect.Left:=Rect.Right-2*dr+1-1;
    end;
  end;
  case SemaphorCaseSensitive of
    false: if (UpperCase(GetEditText(aCol,aRow))=UpperCase(StringGreen))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clGreen;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(UpperCase(GetEditText(aCol,aRow))=UpperCase(StringRed))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clRed;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(UpperCase( GetEditText(aCol,aRow))=UpperCase(StringYellow))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clYellow;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end;
    true: if (GetEditText(aCol,aRow)=StringGreen)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clGreen;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(GetEditText(aCol,aRow)=StringRed)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clRed;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(GetEditText(aCol,aRow)=StringYellow)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clYellow;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end;
  end;
end;

constructor TSemaphorDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Semaphor:=False;
  StringRed:='no';
  StringYellow:='maybe';
  StringGreen:='yes';
  SemaphorShape:=ssDisk;
  SemaphorCaseSensitive:=False;
end;

procedure Register;
begin
  RegisterComponents('Data Controls',[TSemaphorDBGrid]);
end;

initialization
  {$I SemaphorDBGridIcon.lrs}

end.


