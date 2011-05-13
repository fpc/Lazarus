{
                               SemaphorGrid.pas
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
set to true,when TSemaphorGrid detect in a non Fixed Cells a string like
StringGreen or StringYellow or StringRed, it show a colored sign in the
corrispondent cells (shape choosed in SemaphorShape). It can be Case Sensitive
(SemaphorCaseSensitive). If Semaphor is false, nothing happen.

SemaphorGrid is able to store and restore data by indipendent method
LoadFromFileG and SaveToFileG wich manage also accented chars in data and
similar. Data are separeted by CHSEP. LoadFromFileG has autoadjust wich allow
SemaphorGrid to AutosizeColumns. SemaphorGrid, at the moment, is unable to store
setting grid (only Column Hidden and in general ColWidth). With the method
ExportToExcel, SemaphorGrid is able set CHSEP so that the file generated is
MS Excel compatible. SemaphorGrid is also able to sort a column wrapping all
the Grid with the method SortFromColumn with indipendent sorting method (maybe
it should be better to use onCompareCell)
That's all
Enjoy! Salvatore

Date: 15-Jan-2005
- Changed SortFromColumn: now it use SortColRow, OnCompareCells and
  DoCompareChange (from Jesus Rejes A.);
- Removed SortDate, SortNumeric, uses Windows (now useless)
- Correct some repainting problems (from Jesus Rejes A.)
- removed ReDrawGrid (now useless)

Date: 03-Apr-2005
- Some sources cleaning
- introduced System Metrics in AutoWidth and AutoHeight (keep in count
  scrollbars);

Date: 04-May-2005
- set default CHARSEP to #255

knowed bug:
  re-sorting a column that have two or more cells equal, the
  corrispondent rows are swapped, so there are more than one grid sorted by
  the same column.
}

unit SemaphorGrids;
{$mode objfpc}
{$H+}
interface

uses
  Classes, SysUtils, FileUtil, LResources, LCLProc, LCLIntf, LCLType, Forms,
  Controls, Graphics, Dialogs, Grids;

const
  SemaphorMarker='S_M_0_1';

type
  TSheetType=(stLandScape,stPortrait);
  TSemaphorShape=(ssTopBar,ssBottomBar,ssLeftBar,ssRigthBar,
                  ssTopLeftSquare,ssTopRigthSquare,ssBottomLeftSquare,
                  ssBottomRigth,ssDisk);
  TDirection = (sdDescending, sdAscending);
  TTypeSort = (tsAlphabetic, tsDate, tsNumeric, tsAutomatic);

type

  { TSemaphorGrid }

  TSemaphorGrid = class(TStringGrid)
  private
    { Private declarations }
    WidthZero:integer;
    ExWidths: TStringList;
    FAlignment: TAlignment;
    FCHSEP : Char;
    FSemaphor : boolean;
    FStringRed : string;
    FStringYellow : string;
    FStringGreen : string;
    FSemaphorShape : TSemaphorShape;
    FSemaphorCaseSensitive : boolean;
    FSemaphorOnlyFloat : boolean;
    FSortDirection: TDirection;
    FSortType: TTypeSort;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCHSEP(Value : Char);
    procedure SetSemaphor(Value : boolean);
    procedure SetStringRed(Value : string);
    procedure SetStringYellow(Value : string);
    procedure SetStringGreen(Value : string);
    procedure SetSemaphorShape(Value : TSemaphorShape);
    procedure SetSemaphorCaseSensitive(Value : boolean);
    procedure SetSemaphorOnlyFloat(Value : boolean);
  protected
    { Protected declarations }
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    function  DoCompareCells(Acol,ARow,Bcol,BRow: Integer): Integer; override;
    procedure KeyPress(var Key: Char); override;
    procedure LoadBase(tabella:TStringList; autoadjust:boolean);
    procedure SaveBase(tabella:TStringList; addMarker:boolean);
    procedure LoadFromString(StringName:string; autoadjust:boolean);
  public
    { Public declarations }
    procedure LoadFromFileG(FileName:string; autoadjust:boolean);
    procedure SaveToFileG(FileName:String;addMarker:boolean);
    procedure SaveToString(var StringName:String; addMarker:boolean);
    procedure AssignG(SG: TSemaphorGrid; autoadjust:boolean);
    procedure AssignToG(SG: TSemaphorGrid; autoadjust:boolean);
    procedure AutoWidth;
    procedure AutoHeight;
    procedure AutoFit;
    procedure ExportToExcel(FileName:string;SelfExt:boolean);
    procedure DeleteColumn(j:integer);
    procedure DeleteRow(i:integer);
    procedure SortFromColumn(j:integer; TS:TTypeSort; SD:TDirection; autoadjust:boolean);
    procedure HideCol(j:integer);
    procedure ShowCol(j:integer);
    procedure ShowAllCols;
    function Duplicate(var SG:TSemaphorGrid):boolean;
    procedure ClearColRow(isColumn:boolean; i:integer);
    procedure Clear(OnlyValue:boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Constraints;
    property Alignment: TAlignment read  FAlignment write SetAlignment;
    property CHSEP : Char read FCHSEP write SetCHSEP default #255;
    property Semaphor : boolean read FSemaphor write SetSemaphor;
    property StringRed : string read FStringRed write SetStringRed;
    property StringYellow : string read FStringYellow write SetStringYellow;
    property StringGreen : string read FStringGreen write SetStringGreen;
    property SemaphorShape : TSemaphorShape read FSemaphorShape write SetSemaphorShape;
    property SemaphorCaseSensitive : boolean read FSemaphorCaseSensitive write SetSemaphorCaseSensitive;
    property SemaphorOnlyFloat : boolean read FSemaphorOnlyFloat write SetSemaphorOnlyFloat;
  end;

procedure Register;

implementation

procedure TSemaphorGrid.DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState);
const dr=4;
var Rect:TRect;
    MyStyle:TTextStyle;
begin
  PrepareCanvas(aCol,aRow,aState);
  Canvas.FillRect(aRect);
  DrawCellGrid(aCol,aRow,aRect,astate);

  MyStyle:=Canvas.TextStyle;
  MyStyle.Alignment:=Alignment;
  //text space
  aRect.Left:=aRect.Left+dr;
  aRect.Right:=aRect.Right-dr;
  aRect.Bottom:=aRect.Bottom-dr;
  aRect.Top:=aRect.Top+dr;
  Canvas.TextRect(aRect,aRect.Left, aRect.Top, Cells[aCol,aRow],MyStyle);
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
    false: if (UpperCase(Cells[aCol,aRow])=UpperCase(StringGreen))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clGreen;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(UpperCase(Cells[aCol,aRow])=UpperCase(StringRed))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clRed;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(UpperCase(Cells[aCol,aRow])=UpperCase(StringYellow))and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clYellow;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end;
    true: if (Cells[aCol,aRow]=StringGreen)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clGreen;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(Cells[aCol,aRow]=StringRed)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clRed;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end else if(Cells[aCol,aRow]=StringYellow)and((aCol>FixedCols-1)and(aRow>FixedRows-1)) then begin
      Canvas.Brush.Color:=clYellow;
      if not(SemaphorShape=ssDisk) then
        Canvas.Rectangle(Rect)
      else
        Canvas.Ellipse(Rect);
    end;
  end;
end;

procedure TSemaphorGrid.KeyPress(var Key: Char);
var strOld:string;
    valore:double;
begin
  inherited KeyPress(Key);
  if (SemaphorOnlyFloat)and(goEditing in Options) then begin
    if (Key=',')or(Key='.') then
      Key:=DecimalSeparator;
    if (Key=' ')or(UpCase(Key)='E') then
      key:=#0;
    if Key='-' then begin
      strOld:=Cells[Col,Row];
      if Pos(Key,strOld)=1 then
        delete(strOld,1,1)
      else
        strOld:=Key+strOld;
      Cells[Col,Row]:=strOld;
      Key:=#0;
      exit
    end;

    if not(Ord(Key)=VK_BACK) then begin
      if Cells[Col,Row]<>'' then begin
        strOld:=Cells[Col,Row];
        try
          valore:=StrToFloat(strOld+Key)
        except
          Key:=#0;
          exit
        end
      end else begin
        strOld:='';
        try
          valore:=StrToFloat(Cells[Col,Row]+Key)
        except
          Cells[Col,Row]:=strOld;
          Key:=#0
        end;
      end
    end;
  end;
end;

procedure TSemaphorGrid.LoadBase(tabella:TStringList; autoadjust:boolean);
var riga:TStringList;
    strtmp,strFirst:string;
    i,j:integer;
    strj:string;
begin
  riga:=TStringList.Create;
  strFirst:=tabella.Strings[0];
  RowCount:=FixedRows+2;//to prevent grid exception
  ColCount:=FixedCols+2;
  if pos(SemaphorMarker,strFirst)<>0 then begin
    Delete(strFirst,1,pos(CHSEP,strFirst));//delete marker+CHSEP
    j:=pos(CHSEP,strFirst)-1;
    FixedCols:=StrToInt(copy(strFirst,1,j)); //retrive FixedCols
    Delete(strFirst,1,j+1);//pos(CHSEP,strtmp));//delete FixedCols+CHSEP
    i:=pos(CHSEP,strFirst)-1;
    if i=-1 then //i.e. pos(CHSEP,strtmp)=0
      i:=length(strFirst);
    FixedRows:=StrToInt(copy(strFirst,1,i));//retrive FixedCols
    Delete(strFirst,1,i);

    strtmp:='';
    RowCount:=FixedRows+1;
    ColCount:=FixedCols+1;
    for i:=1 to tabella.Count-1 do begin  //riga[0] gia usata per fixed rows  and cols
      strtmp:=tabella.Strings[i];
      riga.Clear;
      j:=0;
      while (strtmp<>'')or(pos(CHSEP,strtmp)<>0)do
        if pos(CHSEP,strtmp)<>0 then begin
          j:=j+1;
          riga.Add(copy(strtmp,1,pos(CHSEP,strtmp)-1));
          Delete(strtmp,1,pos(CHSEP,strtmp))
        end else begin
          riga.Add(strtmp);
          strtmp:=''
        end;
      if RowCount<i then
        RowCount:=RowCount+1;
      if ColCount<j+1 then
        ColCount:=j+1;
      Rows[i-1]:=riga;
    end;
  end else begin
    RowCount:=FixedRows+1;
    ColCount:=FixedCols+1;
    strFirst:='';
    strtmp:='';
    for i:=0 to tabella.Count-1 do begin  //riga[0] gia usata per fixed rows  and cols
      strtmp:=tabella.Strings[i];
      riga.Clear;
      j:=0;
      while (strtmp<>'')or(pos(CHSEP,strtmp)<>0)do
        if pos(CHSEP,strtmp)<>0 then begin
          j:=j+1;
          riga.Add(copy(strtmp,1,pos(CHSEP,strtmp)-1));
          Delete(strtmp,1,pos(CHSEP,strtmp))
        end else begin
          riga.Add(strtmp);
          strtmp:=''
        end;
      if RowCount<i+1 then
        RowCount:=RowCount+1;
      if ColCount<j+1 then
        ColCount:=j+1;
      Rows[i]:=riga;
    end
  end;
  riga.Free;

  if autoadjust then
    AutoAdjustColumns;//all cols, also hidden

  if strFirst<>'' then begin
    ExWidths.Clear;
    while pos(CHSEP+'j',strFirst)<>0 do begin
      Delete(strFirst,1,2);//delete CHSEP+'j';
      strj:=copy(strFirst,1,pos(CHSEP,strFirst)-1);
      ColWidths[StrToInt(strj)]:=WidthZero;//GridLineWidth;
      ExWidths.Add('j'+strj);
      Delete(strFirst,1,length(strj+CHSEP));
      if pos(CHSEP,strFirst)<>0 then begin
        strj:=copy(strFirst,1,pos(CHSEP,strFirst)-1);
        Delete(strFirst,1,pos(CHSEP,strFirst)-1);
      end else
        strj:=strFirst;
      ExWidths.Add(strj);
    end;
    strFirst:='';
  end;
end;

procedure TSemaphorGrid.SaveBase(tabella:TStringList; addMarker:boolean);
var riga:TStringList;
    strtmp:string;
    i,j:integer;
begin
  riga:=TStringList.Create;
  if addMarker then begin
    strtmp:=SemaphorMarker+CHSEP+IntToStr(FixedCols)+CHSEP+IntToStr(FixedRows);//store n° fixed cols and rows
    for j:=0 to ExWidths.Count-1 do //store the widths of hided cols if any (and then the hided cols)
      strtmp:=strtmp+CHSEP+ExWidths.Strings[j];
    tabella.Add(strtmp);
  end;
  for i:=0 to RowCount-1 do begin
    riga.Assign(Rows[i]);
    strtmp:=riga.Strings[0];
    for j:=1 to riga.Count-1 do
      strtmp:=strtmp+CHSEP+riga.Strings[j];
    tabella.Add(strtmp);
  end;
  riga.Free;
end;

procedure TSemaphorGrid.LoadFromFileG(FileName:string;autoadjust:boolean);
var tabella:TStringList;
begin
  tabella:=TStringList.Create;
  tabella.LoadFromFile(UTF8ToSys(Filename));
  LoadBase(tabella,autoadjust);
  tabella.Free;
end;

{ FileName: file to store data }
procedure TSemaphorGrid.SaveToFileG(FileName:String;addMarker:boolean);
var tabella:TStringList;
begin
  tabella:=TStringList.Create;
  SaveBase(tabella,addMarker);
  tabella.SaveToFile(UTF8ToSys(FileName));
  tabella.Free;
end;

procedure TSemaphorGrid.LoadFromString(StringName:string; autoadjust:boolean);
var tabella:TStringList;
begin
  tabella:=TStringList.Create;
  tabella.Text:=StringName;
  LoadBase(tabella,autoadjust);
  tabella.Free;
end;

procedure TSemaphorGrid.SaveToString(var StringName:String; addMarker:boolean);
var tabella:TStringList;
begin
  tabella:=TStringList.Create;
  SaveBase(tabella,addMarker);
  StringName:=tabella.Text;
  tabella.Free;
end;

procedure TSemaphorGrid.AssignG(SG: TSemaphorGrid; autoadjust:boolean);
var strtmp:string;
begin
  SG.SaveToString(strtmp,true);
  LoadFromString(strtmp, autoadjust);
end;

procedure TSemaphorGrid.AssignToG(SG: TSemaphorGrid; autoadjust:boolean);
var strtmp:string;
begin
  SaveToString(strtmp,true);
  SG.LoadFromString(strtmp, autoadjust);
end;

procedure TSemaphorGrid.AutoWidth;
var j,Wtmp:integer;
begin
  Wtmp:=0;
  if BorderStyle=bsSingle then
    {$IFdef MSWindows}
    Wtmp:=Wtmp+2*GetSystemMetrics(SM_CXFIXEDFRAME);
    {$ELSE}
    Wtmp:=Wtmp+2*1;//GetSystemMetrics(SM_CXFIXEDFRAME);
    {$ENDIF}
  for j:=0 to ColCount-1 do
    Wtmp:=Wtmp+GridLineWidth+ColWidths[j];
  Wtmp:=Wtmp-2*GridLineWidth;
  if ScrollBarIsVisible(SB_Vert) then begin
    Wtmp:=Wtmp+GetSystemMetrics(SM_CXVSCROLL);//+GetSystemMetrics(SM_CXEDGE);
  end;
  Width:=Wtmp;
end;

procedure TSemaphorGrid.AutoHeight;
var i,Htmp:integer;
begin
  Htmp:=0;
  if BorderStyle=bsSingle then
    {$IFdef MSWindows}
    Htmp:=Htmp+2*GetSystemMetrics(SM_CYFIXEDFRAME);
    {$ELSE}
    Htmp:=Htmp+2*1;//GetSystemMetrics(SM_CYFIXEDFRAME);
    {$ENDIF}
  for i:=0 to RowCount-1 do
    Htmp:=Htmp+GridLineWidth+RowHeights[i];
  Htmp:=Htmp-2*GridLineWidth;
  if ScrollBarIsVisible(SB_Horz) then begin
    Htmp:=Htmp+GetSystemMetrics(SM_CYVSCROLL);
  end;
  Height:=Htmp;
end;

procedure TSemaphorGrid.AutoFit;
begin
  AutoWidth;
  AutoHeight;
  if not ScrollBarIsVisible(SB_Vert) then
    AutoWidth;
  if not ScrollBarIsVisible(SB_Horz) then
    AutoHeight;
end;

{ FileName: file to export data; SelfExt: if true SemaphorGrid change the file
extension to xls compatible with MS Excel and maybe other similar, and if there
is not extension SemaphorGrid append xls extension to FileName }
procedure TSemaphorGrid.ExportToExcel(FileName:string;SelfExt:boolean);
var CHSEPOld:Char;
    FileNameXLS:string;
    
begin
  CHSEPOld:=CHSEP;
  CHSEP:=#9;//tab
  FileNameXLS:=FileName;
  if (SelfExt)and(UpperCase(ExtractFileExt(FileNameXLS))<>'XLS') then begin
    Delete(FileNameXLS,Length(FileNameXLS)-2,3);//pos(CHSEP,strtmp))
    if Pos('.',FileNameXLS)<>(Length(FileNameXLS)) then
      FileNameXLS:=FileNameXLS+'.xls'
    else
      FileNameXLS:=FileNameXLS+'xls';
  end;
  SaveToFileG(FileNameXLS,false);
  CHSEP:=CHSEPOld
end;

function TSemaphorGrid.DoCompareCells(Acol, ARow, Bcol, BRow: Integer): Integer;
var
  S1,S2: String;
  V1,V2: Extended;
begin
  case FSortType of
    tsAlphabetic:
      begin
        S1 := Cells[ACol,ARow];
        S2 := Cells[BCol,BRow];
        if S1>S2 then Result := 1 else
        if S1<S2 then Result := -1
        else result := 0;
      end;
    tsNumeric, tsDate:
      begin
        if fSortType = tsNumeric then begin
          V1 := StrToFloatDef(Cells[ACol,ARow], 0.0);
          V2 := StrToFloatDef(Cells[BCol,BRow], 0.0);
        end else begin
          V1 := StrToDate(Cells[ACol,ARow]);
          V2 := StrToDate(Cells[BCol,BRow]);
        end;
        if V1>V2 then
          Result := 1
        else if V1<V2 then
          Result := -1
        else
          result := 0;
      end;
  end;
  if FSortDirection=sdDescending then begin
    if Result<0 then result:=1 else
    if result>0 then result:=-1;
  end;
  if assigned(OnCompareCells) then
    OnCompareCells(Self, ACol,ARow,BCol,BRow, Result);
end;

procedure TSemaphorGrid.DeleteColumn(j:integer);
begin
  DeleteColRow(true,j);
end;

procedure TSemaphorGrid.DeleteRow(i:integer);
begin
  DeleteColRow(false,i);
end;

procedure TSemaphorGrid.SortFromColumn(j:integer; TS:TTypeSort; SD:TDirection; autoadjust:boolean);
  function AutomaticSortType: TTypeSort;
  var i: Integer;
  begin
    // returns the sort type of a omogeneus column j
    // for non omogeneus, Alphabetical is assumed
    Result:=tsNumeric;
    for i:=FixedRows to RowCount-1 do
      if Cells[j,i]<>'' then
        try
          StrToFloat(Cells[j,i]);
        except
          Result:=tsDate;
          break;
        end;
    if Result=tsNumeric then
      exit;
    for i:=FixedRows to RowCount-1 do
      if Cells[j,i]<>'' then
        try
          StrToDate(Cells[j,i]);
        except
          Result:=tsAlphabetic;
          break;
        end;
  end;
begin
  if Ts=tsAutomatic then
    FSortType := AutomaticSortType
  else
    FSortType := Ts;
  FSortDirection := SD;
  BeginUpdate;
  SortColRow(True, J);
  if autoadjust then
    AutoAdjustColumns;
  EndUpdate(true);
end;

procedure TSemaphorGrid.HideCol(j:integer);
var strj:string;
begin
  if j<ColCount then begin
    strj:='j'+IntToStr(j);
    if ExWidths.IndexOf(strj)<>-1 then begin
      exit
    end else begin
      ExWidths.Add(strj);
      ExWidths.Add(IntToStr(ColWidths[j]));
    end;
    ColWidths[j]:=WidthZero;
  end;
end;

procedure TSemaphorGrid.ShowCol(j:integer);
var strj:string;
    index:integer;
begin
  if j<ColCount then begin
    strj:='j'+IntToStr(j);
    index:=ExWidths.IndexOf(strj);
    if index<>-1 then begin
      ColWidths[j]:=StrToInt(ExWidths.Strings[index+1]);
      ExWidths.Delete(index+1);
      ExWidths.Delete(index);
    end else
      exit;
  end;
end;

procedure TSemaphorGrid.ShowAllCols;
var j:integer;
    strj:string;
begin
  while ExWidths.Count>0 do begin
    strj:=ExWidths.Strings[0];
    Delete(strj,1,1);
    j:=StrToInt(strj);
    ColWidths[j]:=StrToInt(ExWidths.Strings[1]);
    ExWidths.Delete(1);
    ExWidths.Delete(0);
  end;
(* as different solution
  for j:=0 to ColCount-1 do
    ShowCol(j);
*)
end;

function TSemaphorGrid.Duplicate(var SG:TSemaphorGrid):Boolean;
var i,j:integer;  // da migliorare
    duptmp:Boolean;
begin
  duptmp:=True;
  try
    SG.ColCount:=ColCount;
    SG.RowCount:=RowCount;
    for i:=0 to RowCount-1 do
      for j:=0 to ColCount-1 do
        SG.Cells[j,i]:=Cells[j,i];
  except
    duptmp:=False;
    SG.Clear(false);
  end;
  Result:=duptmp
end;

procedure TSemaphorGrid.ClearColRow(isColumn:boolean; i:integer);
var j:integer;
begin
  if isColumn then
    for j:=0 to RowCount-1 do
      Cells[i,j]:=''
  else
    for j:=0 to ColCount-1 do
      Cells[j,i]:=''
end;

procedure TSemaphorGrid.Clear(OnlyValue:boolean);
var i:integer;
begin
  for i:= 0 to RowCount-1 do
    ClearColRow(false,i);
  if not OnlyValue then begin
    RowCount:=FixedRows+1;
    ColCount:=FixedCols+1
  end
end;

procedure TSemaphorGrid.SetAlignment(Value: TAlignment);
begin
  If FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TSemaphorGrid.SetCHSEP(Value : Char);
begin
  FCHSEP:=Value;
end;

procedure TSemaphorGrid.SetSemaphor(Value : boolean);
begin
  FSemaphor:=Value;
  Invalidate;
end;

procedure TSemaphorGrid.SetStringRed(Value : string);
begin
  FStringRed:=Value;
  Invalidate;
end;

procedure TSemaphorGrid.SetStringYellow(Value : string);
begin
  FStringYellow:=Value;
  Invalidate;
end;

procedure TSemaphorGrid.SetStringGreen(Value : string);
begin
  FStringGreen:=Value;
  Invalidate;
end;

procedure TSemaphorGrid.SetSemaphorShape(Value : TSemaphorShape);
begin
  FSemaphorShape:=Value;
  Invalidate;
end;

procedure TSemaphorGrid.SetSemaphorCaseSensitive(Value : boolean);
begin
  FSemaphorCaseSensitive:=Value;
  invalidate;
end;

procedure TSemaphorGrid.SetSemaphorOnlyFloat(Value : boolean);
begin
  FSemaphorOnlyFloat:=Value;
end;

constructor TSemaphorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCHSEP:=#255;
  Semaphor:=False;
  StringRed:='no';
  StringYellow:='maybe';
  StringGreen:='yes';
  SemaphorShape:=ssDisk;
  SemaphorCaseSensitive:=False;
  SemaphorOnlyFloat:=False;
  Alignment:=taLeftJustify;
  WidthZero:=GridLineWidth;
  ExWidths:=TStringList.Create;
end;

destructor TSemaphorGrid.Destroy;
begin
  ExWidths.Free;
  inherited Destroy
end;

procedure Register;
begin
  RegisterComponents('Additional',[TSemaphorGrid]);
end;

initialization
  {$I SemaphorGridsicon.lrs}

end.

