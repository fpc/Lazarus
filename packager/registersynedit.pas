{  $Id$  }
{
 /***************************************************************************
                           registersynedit.pas
                           -------------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Registration of the SynEdit components.
}
unit RegisterSynEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SynEditLazDsgn, SynEdit, SynCompletion, SynExportHTML, SynMacroRecorder,
  SynMemo, SynHighlighterPas, SynHighlighterCPP, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterHTML, SynHighlighterXML,
  SynHighlighterLFM, SynHighlighterMulti, SynHighlighterUNIXShellScript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterTeX,
  SynHighlighterSQL, SynHighlighterPython, SynHighlighterVB, SynHighlighterAny,
  LazarusPackageIntf, LazarusIDEStrConsts;

procedure Register;

implementation

procedure RegisterSynEditUnit;
begin
  RegisterComponents('SynEdit',[TSynEdit]);
end;

procedure RegisterSynCompletion;
begin
  RegisterComponents('SynEdit',[TSynAutoComplete]);
end;

procedure RegisterSynExportHTML;
begin
  RegisterComponents('SynEdit',[TSynExporterHTML]);
end;

procedure RegisterSynMacroRecorder;
begin
  RegisterComponents('SynEdit',[TSynMacroRecorder]);
end;

procedure RegisterSynMemo;
begin
  RegisterComponents('SynEdit',[TSynMemo]);
end;

procedure RegisterSynHighlighterPas;
begin
  RegisterComponents('SynEdit',[TSynPasSyn, TSynFreePascalSyn]);
end;

procedure RegisterSynHighlighterJava;
begin
  RegisterComponents('SynEdit',[TSynJavaSyn]);
end;

procedure RegisterSynHighlighterCPP;
begin
  RegisterComponents('SynEdit',[TSynCPPSyn]);
end;

procedure RegisterSynHighlighterPerl;
begin
  RegisterComponents('SynEdit',[TSynPerlSyn]);
end;

procedure RegisterSynHighlighterHTML;
begin
  RegisterComponents('SynEdit',[TSynHTMLSyn]);
end;

procedure RegisterSynHighlighterXML;
begin
  RegisterComponents('SynEdit',[TSynXMLSyn]);
end;

procedure RegisterSynHighlighterLFM;
begin
  RegisterComponents('SynEdit',[TSynLFMSyn]);
end;

procedure RegisterSynHighlighterUNIXShellScript;
begin
  RegisterComponents('SynEdit',[TSynUNIXShellScriptSyn]);
end;

procedure RegisterSynHighlighterCSS;
begin
  RegisterComponents('SynEdit',[TSynCssSyn]);
end;

procedure RegisterSynHighlighterPHP;
begin
  RegisterComponents('SynEdit',[TSynPHPSyn]);
end;

procedure RegisterSynHighlighterTeX;
begin
  RegisterComponents('SynEdit',[TSynTeXSyn]);
end;

procedure RegisterSynHighlighterSQL;
begin
  RegisterComponents('SynEdit',[TSynSQLSyn]);
end;

procedure RegisterSynHighlighterPython;
begin
  RegisterComponents('SynEdit',[TSynPythonSyn]);
end;

procedure RegisterSynHighlighterAny;
begin
  RegisterComponents('SynEdit',[TSynAnySyn]);
end;

procedure RegisterSynHighlighterMulti;
begin
  RegisterComponents('SynEdit',[TSynMultiSyn]);
end;

procedure RegisterSynHighlighterVB;
begin
  RegisterComponents('SynEdit',[TSynVBSyn]);
end;

procedure Register;
begin
  RegisterUnit('SynEdit',@RegisterSynEditUnit);
  RegisterUnit('SynCompletion',@RegisterSynCompletion);
  RegisterUnit('SynExportHTML',@RegisterSynExportHTML);
  RegisterUnit('SynMacroRecorder',@RegisterSynMacroRecorder);
  RegisterUnit('SynMemo',@RegisterSynMemo);
  RegisterUnit('SynHighlighterPas',@RegisterSynHighlighterPas);
  RegisterUnit('SynHighlighterCPP',@RegisterSynHighlighterCPP);
  RegisterUnit('SynHighlighterJava',@RegisterSynHighlighterJava);
  RegisterUnit('SynHighlighterPerl',@RegisterSynHighlighterPerl);
  RegisterUnit('SynHighlighterHTML',@RegisterSynHighlighterHTML);
  RegisterUnit('SynHighlighterXML',@RegisterSynHighlighterXML);
  RegisterUnit('SynHighlighterLFM',@RegisterSynHighlighterLFM);
  RegisterUnit('SynHighlighterUNIXShellScript',
                                        @RegisterSynHighlighterUNIXShellScript);
  RegisterUnit('SynHighlighterCss',@RegisterSynHighlighterCSS);
  RegisterUnit('SynHighlighterPHP',@RegisterSynHighlighterPHP);
  RegisterUnit('SynHighlighterTeX',@RegisterSynHighlighterTeX);
  RegisterUnit('SynHighlighterSQL',@RegisterSynHighlighterSQL);
  RegisterUnit('SynHighlighterPython',@RegisterSynHighlighterPython);
  RegisterUnit('SynHighlighterVB',@RegisterSynHighlighterVB);

  RegisterUnit('SynHighlighterAny',@RegisterSynHighlighterAny);
  RegisterUnit('SynHighlighterMulti',@RegisterSynHighlighterMulti);
end;

end.

