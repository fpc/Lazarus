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

{ Default MIME content type information.  Update as necessary. }

type
  TIpDefContentInfo = record
    Typ : string;                { content type, e.g. 'application' }
    Sub : string;                { content subtype, e.g. 'octet-stream' }
    Enc : TIpMimeEncodingMethod; { encoding method, e.g. 'base64' }     {!!.02}
  end;

const
  DefExtensionCount = 14;

  DefExtensions : array[0..(DefExtensionCount-1)] of string =
    ('.bin', '.txt', '.rtf', '.doc', '.ps', '.tif',
     '.gif', '.jpg', '.bmp', '.mov', '.avi', '.wav', '.aif', '.zip');

  DefContent : array[0..(DefExtensionCount-1)] of TIpDefContentInfo = (
    {bin} (Typ : 'application'; Sub : 'octet-stream';     Enc : embase64),
    {txt} (Typ : 'text';        Sub : 'plain';            Enc : em8bit),
    {rtf} (Typ : 'application'; Sub : 'msword';           Enc : em8bit),
    {doc} (Typ : 'application'; Sub : 'msword';           Enc : embase64),
    {ps}  (Typ : 'application'; Sub : 'postscript';       Enc : em8bit),
    {tif} (Typ : 'image';       Sub : 'tif';              Enc : embase64),
    {gif} (Typ : 'image';       Sub : 'gif';              Enc : embase64),
    {jpg} (Typ : 'image';       Sub : 'jpeg';             Enc : embase64),
    {bmp} (Typ : 'image';       Sub : 'bmp';              Enc : embase64),
    {mov} (Typ : 'video';       Sub : 'quicktime';        Enc : embase64),
    {avi} (Typ : 'video';       Sub : 'avi';              Enc : embase64),
    {wav} (Typ : 'video';       Sub : 'wave';             Enc : embase64),
    {aif} (Typ : 'audio';       Sub : 'aif';              Enc : embase64),
    {zip} (Typ : 'application'; Sub : 'x-zip-compressed'; Enc : embase64));

