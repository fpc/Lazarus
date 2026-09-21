{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS utility class

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPCSSUtils;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.TypInfo, System.Classes, System.SysUtils, System.Types, FpCss.Tree, FpCss.Parser, FpCss.Scanner;
{$ELSE FPC_DOTTEDUNITS}
uses
  TypInfo, Classes, SysUtils, Types, fpCSSTree, fpCSSParser, fpCSSScanner;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TCSSClassNameVisitor }

  TCSSClassNameVisitor = Class(TCSSTreeVisitor)
  private
    FList: TStrings;
  public
    Constructor Create(aList: TStrings);
    Procedure Visit(obj: TCSSElement); override;
    property List : TStrings Read FList;
  end;

  { TCSSUtils }

  TCSSUtils = class(TComponent)
  private
    FExtraScannerOptions: TCSSScannerOptions;
  published
    Procedure ExtractClassNames(Const aFileName : String; aList : TStrings);
    Procedure ExtractClassNames(Const aStream : TStream; aList : TStrings);
    Procedure ExtractClassNames(Const aElement : TCSSElement; aList : TStrings);
    Function ExtractClassNames(Const aFileName : String) : TStringDynArray;
    Function ExtractClassNames(Const aStream : TStream) : TStringDynArray;
    Function ExtractClassNames(Const aElement : TCSSElement) : TStringDynArray;
    Procedure Minimize(aInput,aOutput : TStream);
    Property ExtraScannerOptions : TCSSScannerOptions Read FExtraScannerOptions Write FExtraScannerOptions;
  end;

implementation

{ TCSSClassNameVisitor }

constructor TCSSClassNameVisitor.Create(aList: TStrings);
begin
  FList:=aList;
end;

procedure TCSSClassNameVisitor.Visit(obj: TCSSElement);
begin
  if Obj.CSSType=csstCLASSNAME then
    FList.Add(Obj.AsString);
end;

{ TCSSUtils }

procedure TCSSUtils.ExtractClassNames(const aFileName: String; aList: TStrings);

Var
  S : TStringStream;

begin
  S:=TStringStream.Create;
  try
    S.LoadFromFile(aFileName);
    ExtractClassNames(S,aList);
  finally
    S.Free;
  end;
end;

function TCSSUtils.ExtractClassNames(const aFileName: String): TStringDynArray;

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    ExtractClassNames(aFileName,L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;
end;

procedure TCSSUtils.ExtractClassNames(const aStream: TStream; aList: TStrings);

Var
  aParser : TCSSParser;
  aElement : TCSSElement;

begin
  aElement:=Nil;
  aParser:=TCSSParser.Create(aStream,ExtraScannerOptions);
  try
    aElement:=aParser.Parse;
    ExtractClassNames(aElement,aList);
  finally
    aElement.Free;
    aParser.Free;
  end;
end;

procedure TCSSUtils.ExtractClassNames(const aElement: TCSSElement; aList: TStrings);

Var
  aVis : TCSSClassNameVisitor;

begin
  aVis:=TCSSClassNameVisitor.Create(aList);
  try
    aElement.Iterate(aVis);
  finally
    aVis.Free;
  end;
end;

function TCSSUtils.ExtractClassNames(const aStream: TStream): TStringDynArray;

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    ExtractClassNames(aStream,L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;
end;

function TCSSUtils.ExtractClassNames(const aElement: TCSSElement): TStringDynArray;

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    ExtractClassNames(aElement,L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;

end;

procedure TCSSUtils.Minimize(aInput, aOutput: TStream);

Var
  aScanner : TCSSScanner;
  aToken,aPreviousToken : TCSSToken;
  S : UTF8String;

begin
  aPrevioustoken:=ctkWHITESPACE;
  AScanner:=TCSSScanner.Create(aInput);
  try
    aScanner.ReturnWhiteSpace:=True;
    aToken:=aScanner.FetchToken;
    While (aToken<>ctkEOF) do
      begin
      if aToken=ctkSTRING then
        S:=StringToCSSString(aScanner.CurTokenString)
      else if aToken<>ctkWHITESPACE then
        S:=aScanner.CurTokenString
      else if aPreviousToken<>ctkWHITESPACE then
        S:=' '
      else
        S:='';
      // writeln(GetEnumName(TypeInfo(TCSSTOKEN),Ord(aToken)),' -> S : >',S,'<');
      if S<>'' then
        aOutput.WriteBuffer(S[1],length(S));
      aPreviousToken:=aToken;
      aToken:=aScanner.FetchToken;
      end;

  finally
    aScanner.Free;
  end;
end;

end.

