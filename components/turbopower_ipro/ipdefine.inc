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

{$MODE DELPHI}{$H+}

// Activate several features of the package by enabling the following DEFINEs.
// Best flexibility is achieved by doing this in the "Additions and Overrides"
// of the Project options.

// Uncomment if you want to enable printing functionality

//{$DEFINECSS_CASESENSITIVE_CLASSID}

//{$DEFINE IP_LAZARUS_DBG}

// (requires extra build/runtime dependencies for Linux like cairo,etc)
//{$DEFINE Html_print}

// Uncomment if you want to disable RTTI access to properties of the HTML nodes
//{$DEFINE Html_RTTI}

// TODO removing it from source code or enable it
//{$UNDEF UseGifImageUnit}
//{$UNDEF AndersGIFImage}
//{$UNDEF ImageLibGIFImage}
//{$UNDEF UsePNGGraphic}
//{$DEFINE HtmlWithoutHttp}
