{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditReg.pas, released 2000-04-07.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditReg;

{$I SynEdit.inc}

interface

uses
  Classes, SynEditStrConst, SynEditPropertyReg,
// SynEdit components
  SynEdit, SynMemo, SynDBEdit, SynExportHTML, SynExportRTF,
  SynHighlighterMulti, SynCompletionProposal, SynEditPythonBehaviour,
  SynEditPrint, SynEditPrintPreview,
{$IFDEF SYN_COMPILER_4_UP}
  SynHighlighterManager,
{$ENDIF}
// SynEdit highlighters
  SynHighlighterADSP21xx, SynHighlighterAsm, SynHighlighterAWK,
  SynHighlighterBaan, SynHighlighterBat, SynHighlighterCAC, SynHighlighterCache,
  SynHighlighterCpp, SynHighlighterCss, SynHighlighterDfm, SynHighlighterDml,
  SynHighlighterFortran, SynHighlighterFoxpro, SynHighlighterGalaxy,
  SynHighlighterGeneral, SynHighlighterHC11, SynHighlighterHP48,
  SynHighlighterHtml, SynHighlighterIni, SynHighlighterInno, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterKix, SynHighlighterPas,
  SynHighlighterPerl, SynHighlighterPHP, SynHighlighterProgress,
  SynHighlighterPython, SynHighlighterSml, SynHighlighterSql,
  SynHighlighterTclTk, SynHighlighterVB, SynHighlighterVBScript;

procedure Register;

implementation

procedure Register;
begin
  SynEditPropertyReg.Register;
// SynEdit components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit, TSynMemo, TDBSynEdit,
    TSynExporterHTML, TSynExporterRTF, TSynEditPythonBehaviour, TSynMultiSyn, 
    TSynCompletionProposal, TSynAutoComplete,
    TSynEditPrint, TSynEditPrintPreview]);
// SynEdit highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynCppSyn, TSynFortranSyn, TSynGeneralSyn, TSynJavaSyn, TSynPasSyn,
    TSynVBSyn,
    // internet
    TSynCssSyn, TSynHTMLSyn, TSynJScriptSyn, TSynPHPSyn, TSynVBScriptSyn,
    //interpreted
    TSynAWKSyn, TSynBATSyn, TSynKixSyn, TSynPerlSyn, TSynPythonSyn,
    TSynTclTkSyn, 
    //database
    TSynCACSyn, TSynCacheSyn, TSynFoxproSyn, TSynSQLSyn,
    //assembler
    TSynADSP21xxSyn, TSynAsmSyn, TSynHC11Syn, TSynHP48Syn,
    //data modeling
    TSynDmlSyn, TSynSMLSyn,
    //data
    TSynDfmSyn, TSynIniSyn, TSynInnoSyn,
    // other
    TSynBaanSyn, TSynGalaxySyn, TSynProgressSyn
  ]);
end;

end.
