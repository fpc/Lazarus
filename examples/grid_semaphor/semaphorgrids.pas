{
                               SemaphorGrid.pas
                             -------------------
                             Lazarus LCL Component
                          First Release: January 2005

  Author: Salvatore Coppola - Calabria (Italy)
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
}
unit SemaphorGrids;
{$mode objfpc}
{$H+}
interface

uses
{$ifdef win32}Windows,{$endif win32} Classes, SysUtils,
  LResources, LCLtype, Forms, Controls, Graphics, Dialogs, Grids;

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
    procedure AutoInitialize;
    procedure SetAlignment(Value: TAlignment);
    function GetAlignment: TAlignment;
    function GetCHSEP : Char;
    procedure SetCHSEP(Value : Char);
    function GetSemaphor : boolean;
    procedure SetSemaphor(Value : boolean);
    function GetStringRed : string;
    procedure SetStringRed(Value : string);
    function GetStringYellow : string;
    procedure SetStringYellow(Value : string);
    function GetStringGreen : string;
    procedure SetStringGreen(Value : string);
    function GetSemaphorShape : TSemaphorShape;
    procedure SetSemaphorShape(Value : TSemaphorShape);
    function GetSemaphorCaseSensitive : boolean;
    procedure SetSemaphorCaseSensitive(Value : boolean);
    function GetSemaphorOnlyFloat : boolean;
    procedure SetSemaphorOnlyFloat(Value : boolean);
  protected
    { Protected declarations }
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SortDate(var SL: TStringList);
    procedure SortNumeric(var SL: TStringList);
  public
    { Public declarations }
    procedure LoadFromFileG(FileName:string;autoadjust:boolean);
    procedure SaveToFileG(FileName:String;addMarker:boolean);
    procedure AutoWidth;
    procedure AutoHeight;
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
    procedure ReDrawGrid;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Constraints;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property CHSEP : Char read GetCHSEP write SetCHSEP;
    property Semaphor : boolean read GetSemaphor write SetSemaphor;
    property StringRed : string read GetStringRed write SetStringRed;
    property StringYellow : string read GetStringYellow write SetStringYellow;
    property StringGreen : string read GetStringGreen write SetStringGreen;
    property SemaphorShape : TSemaphorShape read GetSemaphorShape write SetSemaphorShape;
    property SemaphorCaseSensitive : boolean read GetSemaphorCaseSensitive write SetSemaphorCaseSensitive;
    property SemaphorOnlyFloat : boolean read GetSemaphorOnlyFloat write SetSemaphorOnlyFloat;
  end;

procedure Register;

implementation

procedure TSemaphorGrid.SortDate(var SL: TStringList);
var i,j:integer;
    date1, date2:TDate;
    str1,str2:string;
    founded:boolean;
begin
  for i:=0 to SL.Count-2 do begin
    j:=i+1;
    str1:=SL.Strings[j];
    date1:=StrToDate(Copy(str1,1,pos(CHSEP,str1)-1));
    founded:=false;
    while (j<>0)and(not founded)do begin
      str2:=SL.Strings[j-1];
      date2:=StrToDate(Copy(str2,1,pos(CHSEP,str2)-1));
      if date1>=date2 then
        founded:=true
      else begin
        SL.Strings[j]:=SL.Strings[j-1];
        j:=j-1
      end;
      SL.Strings[j]:=str1;
    end;
  end;
end;

procedure TSemaphorGrid.SortNumeric(var SL: TStringList);
var i,j:integer;
    num1, num2:double;
    str1,str2:string;
    strn1,strn2:string;
    founded:boolean;
begin
  for i:=0 to SL.Count-2 do begin
    j:=i+1;
    str1:=SL.Strings[j];
    strn1:=Copy(str1,1,pos(CHSEP,str1)-1);
    try
      num1:=StrToFloat(strn1);
    except
      num1:=0;
    end;
    founded:=false;
    while (j<>0)and(not founded)do begin
      str2:=SL.Strings[j-1];
      strn2:=Copy(str2,1,pos(CHSEP,str2)-1);
      try
        num2:=StrToFloat(strn2);
      except
        num2:=0;
      end;
      if num1>=num2 then
        founded:=true
      else begin
        SL.Strings[j]:=SL.Strings[j-1];
        j:=j-1
      end;
      SL.Strings[j]:=str1;
    end;
  end;
end;

procedure TSemaphorGrid.DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState);
const
  dr=4;
var Rect:TRect;
    MyStyle:TTextStyle;
begin
  PrepareCanvas(aCol,aRow,aState);
  Canvas.FillRect(aRect);
  DrawCellGrid(aCol,aRow,aRect,astate);

  MyStyle:=Canvas.TextStyle;
  MyStyle.Alignment:=Alignment;
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

procedure TSemaphorGrid.LoadFromFileG(FileName:string;autoadjust:boolean);
var i,j:integer;
    strtmp,strFirst:string;
    tabella,riga:TStringList;
    strj:string;
begin
  tabella:=TStringList.Create;
  riga:=TStringList.Create;
  tabella.LoadFromFile(Filename);

  strFirst:=tabella.Strings[0];
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
    end
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
  tabella.Free;
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

{ FileName: file to store data }
procedure TSemaphorGrid.SaveToFileG(FileName:String;addMarker:boolean);
var i,j:integer;
    strtmp:string;
    tabella,riga:TStringList;
begin
  tabella:=TStringList.Create;
  riga:=TStringList.Create;
  if addMarker then begin
    strtmp:=SemaphorMarker+CHSEP+IntToStr(FixedCols)+CHSEP+IntToStr(FixedRows);//store n° fixed cols and rows
    for j:=0 to ExWidths.Count-1 do //store the widths of hidden cols if any (and then the hidden cols)
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
  tabella.SaveToFile(FileName);
  riga.Free;
  tabella.Free;
end;

procedure TSemaphorGrid.AutoWidth;
const dx=3;
var j:integer;
begin
  Width:=0;
  if BorderStyle=bsSingle then
    Width:=Width+2*dx;
  for j:=0 to ColCount-1 do
    Width:=Width+ColWidths[j];
end;

procedure TSemaphorGrid.AutoHeight;
const dy=3;
var i:integer;
begin
  Height:=0;
  if BorderStyle=bsSingle then
    Height:=Height+2*dy;
  for i:=0 to RowCount-1 do
    Height:=Height+RowHeights[i];
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

procedure TSemaphorGrid.DeleteColumn(j:integer);
begin
  DeleteColRow(true,j);
end;

procedure TSemaphorGrid.DeleteRow(i:integer);
begin
  DeleteColRow(false,i);
end;

procedure TSemaphorGrid.SortFromColumn(j:integer; TS:TTypeSort; SD:TDirection; autoadjust:boolean);
label uscita;
const basename='namerow';
      FileTemp='Filetemp.tmp';
var i,fr:integer;
    valNum:double;
    valDate:TDate;
    strNomeFiletmp,WinTemp:string;
    MyStringsList:TStringList;
{$ifndef linux}
    buffer: array [0..Max_path] of Char;
{$endif linux}
begin
  if RowCount-FixedRows<=1 then
    exit;
  if TS=tsAutomatic then begin
    TS:=tsAlphabetic;
    for i:=FixedRows to RowCount-1 do begin
      if Cells[j,i]<>'' then
        try
          valNum:=StrToFloat(Cells[j,i]);
        except
          break;
        end;
      TS:=tsNumeric;
      goto uscita;
    end;
    for i:=FixedRows to RowCount-1 do begin
      if Cells[j,i]<>'' then
        try
          valDate:=StrToDate(Cells[j,i]);
        except
          break;
        end;
      TS:=tsDate;
      goto uscita;
    end;
  end;
uscita:
{$ifdef linux}WinTemp:='/tmp';{$else linux}
  GetTempPath(SizeOf(buffer),buffer);//uses Windows
  WinTemp:=buffer;
{$endif linux}
  fr:=FixedRows; //save the FixedRows begin
  for i:=1 to fr do begin
    strNomeFiletmp:=WinTemp+basename+IntToStr(i)+'.tmp';
    Rows[0].SaveToFile(strNomeFiletmp);
    DeleteRow(0);
  end;//save the FixedRows end

  ExchangeColRow(true,0,j);

  MyStringsList:=TStringList.Create;
  SaveToFileG(WinTemp+FileTemp,false);
  MyStringsList.LoadFromFile(WinTemp+FileTemp);
  case TS of
    tsAlphabetic: MyStringsList.Sort;
    tsDate: SortDate(MyStringsList);
    tsNumeric: SortNumeric(MyStringsList);
  end;

  if SD=sdDescending then
    for i:=0 to ((MyStringsList.Count-1)div 2) do
      MyStringsList.Exchange(i,MyStringsList.Count-1-i);

  MyStringsList.SaveToFile(WinTemp+FileTemp);
  LoadFromFileG(WinTemp+FileTemp,autoadjust);
  DeleteFile(pchar(WinTemp+FileTemp));
  MyStringsList.Free;

  ExchangeColRow(true,0,j);
  for i:=fr downto 1 do begin   //restore the FixedRows begin
    RowCount:=RowCount+1;
    strNomeFiletmp:=WinTemp+basename+IntToStr(i)+'.tmp';
    MyStringsList:=TStringList.Create;
    MyStringsList.LoadFromFile(strNomeFiletmp);
    Rows[RowCount-1]:=MyStringsList;
    MyStringsList.Free;
    MoveColRow(false,RowCount-1,0);
    DeleteFile(PChar(strNomeFiletmp));
  end;                          //restore the FixedRows end
  if autoadjust then
    AutoAdjustColumns;
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
(* alternativa
  for j:=0 to ColCount-1 do
    ShowCol(j);
*)
end;

function TSemaphorGrid.Duplicate(var SG:TSemaphorGrid):Boolean;
var i,j:integer;  //from Coppola da migliorare
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

procedure TSemaphorGrid.ReDrawGrid;
var aCol,aRow:integer;
    aRect:TRect;
    aState:TGridDrawState;
begin
  for aRow:=0 to RowCount-1 do
    for aCol:=0 to ColCount-1 do begin
      aRect:=CellRect(aCol,aRow);
      aState:=[];
      if (aCol<FixedCols)or(aRow<FixedRows) then
        aState:=aState+[gdFixed];
      if (aCol=Col)and(aRow=Row) then
        aState:=aState+[gdFocused];
      DrawCell(aCol,aRow,aRect,aState);
    end
end;

procedure TSemaphorGrid.AutoInitialize;
begin
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

procedure TSemaphorGrid.SetAlignment(Value: TAlignment);
begin
  If FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TSemaphorGrid.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TSemaphorGrid.GetCHSEP : Char;
begin
  Result:=FCHSEP;
end;

procedure TSemaphorGrid.SetCHSEP(Value : Char);
begin
  FCHSEP:=Value;
end;

function TSemaphorGrid.GetSemaphor : boolean;
begin
  Result:=FSemaphor;
end;

procedure TSemaphorGrid.SetSemaphor(Value : boolean);
begin
  FSemaphor:=Value;
end;

function TSemaphorGrid.GetStringRed : string;
begin
  Result:=FStringRed;
end;

procedure TSemaphorGrid.SetStringRed(Value : string);
begin
  FStringRed:=Value;
end;

function TSemaphorGrid.GetStringYellow : string;
begin
  Result:=FStringYellow;
end;

procedure TSemaphorGrid.SetStringYellow(Value : string);
begin
  FStringYellow:=Value;
end;

function TSemaphorGrid.GetStringGreen : string;
begin
  Result:=FStringGreen;
end;

procedure TSemaphorGrid.SetStringGreen(Value : string);
begin
  FStringGreen:=Value;
end;

function TSemaphorGrid.GetSemaphorShape : TSemaphorShape;
begin
  Result:=FSemaphorShape;
end;

procedure TSemaphorGrid.SetSemaphorShape(Value : TSemaphorShape);
begin
  FSemaphorShape:=Value;
end;

function TSemaphorGrid.GetSemaphorCaseSensitive : boolean;
begin
  Result:=FSemaphorCaseSensitive;
end;

procedure TSemaphorGrid.SetSemaphorCaseSensitive(Value : boolean);
begin
  FSemaphorCaseSensitive:=Value;
end;

function TSemaphorGrid.GetSemaphorOnlyFloat : boolean;
begin
  Result:=FSemaphorOnlyFloat;
end;

procedure TSemaphorGrid.SetSemaphorOnlyFloat(Value : boolean);
begin
  FSemaphorOnlyFloat:=Value;
end;

constructor TSemaphorGrid.Create(AOwner: TComponent);
begin
  AutoInitialize;
  inherited Create(AOwner);
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


