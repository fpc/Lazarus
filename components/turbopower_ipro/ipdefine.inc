{******************************************************************}
{*     IPDEFINE.INC - Include file for compiler defines           *}
{******************************************************************}

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
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{NOTE: IPDEFINE.INC is included in all Internet Professional units,
       so you can specify global compiler options here. IPDEFINE.INC
       is included *before* each unit's own required compiler options,
       so options specified here could be overridden by hardcoded
       options in the unit source file.}

{$IFDEF LCL}
{$DEFINE IP_LAZARUS}
{$ENDIF}


{$IFDEF IP_LAZARUS}

{$MODE DELPHI}{$H+}

{$DEFINE VERSION7}          { Delphi 7.0 or higher }
{$DEFINE Version6OrHigher}
{$DEFINE Version7OrHigher}

{$ELSE}

{====Compiler options that can be changed====}
{$A+ Force alignment on word/dword boundaries}
{$S- No stack checking}

{====Global fixed compiler options (do NOT change)====}

{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$Q- No arithmetic overflow checking}
{$R- No range checking}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are byte sized}

{====General defines=================================================}

{.$DEFINE UseGifImageUnit}

{$IFDEF VER110}
  {.$DEFINE UseJPegUnit}  {BCB3 does not include the JPeg unit by default}
{$ELSE}
  {$DEFINE UseJPegUnit}
{$ENDIF}

// Define the following symbol to use the TIpAnimatedGIFImage class.  To use
// this class, you must have Ander's TGIFImage.
{.$DEFINE AndersGIFImage }

// Define the following symbol to use the TIpAnimatedImageLibImage.  To use
// this class, you must have the ImageLib ILWebImage component.
{.$DEFINE ImageLibGIFImage }

// Define this symbol to use the TIpPNGImage class.  TIpPNGImage requires the
// ZLib library.
{$DEFINE UsePNGGraphic }

// !!.12 Define this symbol to exclude all support for HTTP and sockets in
// an application using the HTML panels, e.g. for HTML based wizards
{.$DEFINE HtmlWithoutHttp}

// we do not want to include PNG support when building the design-time package
// (that would cause problems if another package also included ZLib).  We also
// do not want to include units that aren't ours, so we'll undefine the other
// graphics stuff too. When rebuilding the design-time package, the
// 'BuildingDesignTime' define must be defined
{$IFDEF BuildingDesignTime}
  {$UNDEF UseGifImageUnit}
  {$UNDEF AndersGIFImage}
  {$UNDEF ImageLibGIFImage}
  {$UNDEF UsePNGGraphic}
  {$DEFINE HtmlWithoutHttp}
{$ENDIF}

{-------- !! DO NOT CHANGE DEFINES BELOW THIS LINE !! --------}

{ Compiler stoppers for unsupported compilers }

{$IFDEF VER80}
  !!! Internet Professional does not support Delphi 1
{$ENDIF}

{$IFDEF VER90}
  !!! Internet Professional does not support Delphi 2
{$ENDIF}

{$IFDEF VER93}
  !!! Internet Professional does not support C++Builder 1
{$ENDIF}

{$DEFINE VERSION3}            { Delphi 3.0 or BCB 3.0 or higher }

{ General define indicating use under C++ Builder }

{$IFDEF VER110}               { C++ Builder 3 }
 {$DEFINE CBuilder}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125}               { C++ Builder 4 }
 {$DEFINE CBuilder}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130}               { Delphi 5 & C++ Builder 5 }
 {$IFDEF BCB}                 { C++ Builder 5 }
  {$DEFINE CBuilder}
  {$ObjExportAll On}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER140}               { Delphi 6 & C++Builder 6 }
  {$IFDEF BCB}                { C++ Builder 6}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}             { Delphi 3 }
  {$IFNDEF VER110}            { BCB 3 }
   {$DEFINE VERSION4}         { Delphi 4.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION4}
 {$IFNDEF VER120}             { Delphi 4 }
  {$IFNDEF VER125}            { BCB 4 }
   {$DEFINE VERSION5}         { Delphi 5.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VERSION4}
  {$DEFINE VERSION3ONLY}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
  {$IFDEF VER140}
     {$DEFINE VERSION6}         { Delphi 6.0 or higher }
     {$DEFINE Version6OrHigher}
  {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
  {$IFDEF VER150}
    {$DEFINE VERSION7}          { Delphi 7.0 or higher }
    {$DEFINE Version6OrHigher}
    {$DEFINE Version7OrHigher}
  {$ENDIF}
{$ENDIF}

{$IFDEF VERSION7}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$ENDIF}
