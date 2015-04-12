{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

(*
 visit the following URL for more information
 http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
 http://unicode.org/reports/tr11/
*)

unit SynEditTextDoubleWidthChars;

{$I synedit.inc}

interface

uses
  {$ifdef windows}{$IFDEF SynForceDoubeWidthHack} Windows, {$endif}{$endif}
  Classes, SysUtils, LazSynEditText;

type

  { SynEditTextDoubleWidthChars }

  SynEditStringDoubleWidthChars = class(TSynEditStringsLinked)
  protected
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  end;


implementation

{$IFDEF SynForceDoubeWidthHack}
type
{  For more information, see UAX #11: East Asian Width,
   at http://www.unicode.org/reports/tr11/
  EastAsianWidth-7.0.0.txt
# Date: 2014-02-28, 23:15:00 GMT [KW, LI]
# This file is an informative contributory data file in the
# Unicode Character Database.
}
  TCharWidth = (cwN, cwA, cwH, cwW, cwF, cwNa);
  TCharRange = record
    l, h  : Integer;
    l8    : Int64; // utf8 encoding. UTF8 character is no longer that 6-bytes
    h8    : Int64; // utf8 encoding
    w     : TCharWidth;
  end;

var
  cjkarr  : array of TCharRange = nil;

procedure InitCJKWidth;
begin
  // Copyright (c) 1991-2014 Unicode, Inc.
  // For terms of use, see http://www.unicode.org/terms_of_use.html
  // this part is automatically generated based on EastAsianWidth.txt
  SetLength(cjkarr,222);
  SetLength(cjkarr,222);
  with cjkarr[0] do begin l:=32; h:=126; w:=cwNa; l8:=$20; h8:=$7E; end;
  with cjkarr[1] do begin l:=161; h:=161; w:=cwA; l8:=$C2A1; h8:=$C2A1; end;
  with cjkarr[2] do begin l:=162; h:=163; w:=cwNa; l8:=$C2A2; h8:=$C2A3; end;
  with cjkarr[3] do begin l:=164; h:=164; w:=cwA; l8:=$C2A4; h8:=$C2A4; end;
  with cjkarr[4] do begin l:=165; h:=166; w:=cwNa; l8:=$C2A5; h8:=$C2A6; end;
  with cjkarr[5] do begin l:=167; h:=168; w:=cwA; l8:=$C2A7; h8:=$C2A8; end;
  with cjkarr[6] do begin l:=170; h:=170; w:=cwA; l8:=$C2AA; h8:=$C2AA; end;
  with cjkarr[7] do begin l:=172; h:=172; w:=cwNa; l8:=$C2AC; h8:=$C2AC; end;
  with cjkarr[8] do begin l:=173; h:=174; w:=cwA; l8:=$C2AD; h8:=$C2AE; end;
  with cjkarr[9] do begin l:=175; h:=175; w:=cwNa; l8:=$C2AF; h8:=$C2AF; end;
  with cjkarr[10] do begin l:=176; h:=180; w:=cwA; l8:=$C2B0; h8:=$C2B4; end;
  with cjkarr[11] do begin l:=182; h:=186; w:=cwA; l8:=$C2B6; h8:=$C2BA; end;
  with cjkarr[12] do begin l:=188; h:=191; w:=cwA; l8:=$C2BC; h8:=$C2BF; end;
  with cjkarr[13] do begin l:=198; h:=198; w:=cwA; l8:=$C386; h8:=$C386; end;
  with cjkarr[14] do begin l:=208; h:=208; w:=cwA; l8:=$C390; h8:=$C390; end;
  with cjkarr[15] do begin l:=215; h:=216; w:=cwA; l8:=$C397; h8:=$C398; end;
  with cjkarr[16] do begin l:=222; h:=225; w:=cwA; l8:=$C39E; h8:=$C3A1; end;
  with cjkarr[17] do begin l:=230; h:=230; w:=cwA; l8:=$C3A6; h8:=$C3A6; end;
  with cjkarr[18] do begin l:=232; h:=234; w:=cwA; l8:=$C3A8; h8:=$C3AA; end;
  with cjkarr[19] do begin l:=236; h:=237; w:=cwA; l8:=$C3AC; h8:=$C3AD; end;
  with cjkarr[20] do begin l:=240; h:=240; w:=cwA; l8:=$C3B0; h8:=$C3B0; end;
  with cjkarr[21] do begin l:=242; h:=243; w:=cwA; l8:=$C3B2; h8:=$C3B3; end;
  with cjkarr[22] do begin l:=247; h:=250; w:=cwA; l8:=$C3B7; h8:=$C3BA; end;
  with cjkarr[23] do begin l:=252; h:=252; w:=cwA; l8:=$C3BC; h8:=$C3BC; end;
  with cjkarr[24] do begin l:=254; h:=254; w:=cwA; l8:=$C3BE; h8:=$C3BE; end;
  with cjkarr[25] do begin l:=257; h:=257; w:=cwA; l8:=$C481; h8:=$C481; end;
  with cjkarr[26] do begin l:=273; h:=273; w:=cwA; l8:=$C491; h8:=$C491; end;
  with cjkarr[27] do begin l:=275; h:=275; w:=cwA; l8:=$C493; h8:=$C493; end;
  with cjkarr[28] do begin l:=283; h:=283; w:=cwA; l8:=$C49B; h8:=$C49B; end;
  with cjkarr[29] do begin l:=294; h:=295; w:=cwA; l8:=$C4A6; h8:=$C4A7; end;
  with cjkarr[30] do begin l:=299; h:=299; w:=cwA; l8:=$C4AB; h8:=$C4AB; end;
  with cjkarr[31] do begin l:=305; h:=307; w:=cwA; l8:=$C4B1; h8:=$C4B3; end;
  with cjkarr[32] do begin l:=312; h:=312; w:=cwA; l8:=$C4B8; h8:=$C4B8; end;
  with cjkarr[33] do begin l:=319; h:=322; w:=cwA; l8:=$C4BF; h8:=$C582; end;
  with cjkarr[34] do begin l:=324; h:=324; w:=cwA; l8:=$C584; h8:=$C584; end;
  with cjkarr[35] do begin l:=328; h:=331; w:=cwA; l8:=$C588; h8:=$C58B; end;
  with cjkarr[36] do begin l:=333; h:=333; w:=cwA; l8:=$C58D; h8:=$C58D; end;
  with cjkarr[37] do begin l:=338; h:=339; w:=cwA; l8:=$C592; h8:=$C593; end;
  with cjkarr[38] do begin l:=358; h:=359; w:=cwA; l8:=$C5A6; h8:=$C5A7; end;
  with cjkarr[39] do begin l:=363; h:=363; w:=cwA; l8:=$C5AB; h8:=$C5AB; end;
  with cjkarr[40] do begin l:=462; h:=462; w:=cwA; l8:=$C78E; h8:=$C78E; end;
  with cjkarr[41] do begin l:=464; h:=464; w:=cwA; l8:=$C790; h8:=$C790; end;
  with cjkarr[42] do begin l:=466; h:=466; w:=cwA; l8:=$C792; h8:=$C792; end;
  with cjkarr[43] do begin l:=468; h:=468; w:=cwA; l8:=$C794; h8:=$C794; end;
  with cjkarr[44] do begin l:=470; h:=470; w:=cwA; l8:=$C796; h8:=$C796; end;
  with cjkarr[45] do begin l:=472; h:=472; w:=cwA; l8:=$C798; h8:=$C798; end;
  with cjkarr[46] do begin l:=474; h:=474; w:=cwA; l8:=$C79A; h8:=$C79A; end;
  with cjkarr[47] do begin l:=476; h:=476; w:=cwA; l8:=$C79C; h8:=$C79C; end;
  with cjkarr[48] do begin l:=593; h:=593; w:=cwA; l8:=$C991; h8:=$C991; end;
  with cjkarr[49] do begin l:=609; h:=609; w:=cwA; l8:=$C9A1; h8:=$C9A1; end;
  with cjkarr[50] do begin l:=708; h:=708; w:=cwA; l8:=$CB84; h8:=$CB84; end;
  with cjkarr[51] do begin l:=711; h:=711; w:=cwA; l8:=$CB87; h8:=$CB87; end;
  with cjkarr[52] do begin l:=713; h:=715; w:=cwA; l8:=$CB89; h8:=$CB8B; end;
  with cjkarr[53] do begin l:=717; h:=717; w:=cwA; l8:=$CB8D; h8:=$CB8D; end;
  with cjkarr[54] do begin l:=720; h:=720; w:=cwA; l8:=$CB90; h8:=$CB90; end;
  with cjkarr[55] do begin l:=728; h:=731; w:=cwA; l8:=$CB98; h8:=$CB9B; end;
  with cjkarr[56] do begin l:=733; h:=733; w:=cwA; l8:=$CB9D; h8:=$CB9D; end;
  with cjkarr[57] do begin l:=735; h:=735; w:=cwA; l8:=$CB9F; h8:=$CB9F; end;
  with cjkarr[58] do begin l:=768; h:=879; w:=cwA; l8:=$CC80; h8:=$CDAF; end;
  with cjkarr[59] do begin l:=913; h:=929; w:=cwA; l8:=$CE91; h8:=$CEA1; end;
  with cjkarr[60] do begin l:=931; h:=937; w:=cwA; l8:=$CEA3; h8:=$CEA9; end;
  with cjkarr[61] do begin l:=945; h:=961; w:=cwA; l8:=$CEB1; h8:=$CF81; end;
  with cjkarr[62] do begin l:=963; h:=969; w:=cwA; l8:=$CF83; h8:=$CF89; end;
  with cjkarr[63] do begin l:=1025; h:=1025; w:=cwA; l8:=$D081; h8:=$D081; end;
  with cjkarr[64] do begin l:=1040; h:=1103; w:=cwA; l8:=$D090; h8:=$D18F; end;
  with cjkarr[65] do begin l:=1105; h:=1105; w:=cwA; l8:=$D191; h8:=$D191; end;
  with cjkarr[66] do begin l:=4352; h:=4447; w:=cwW; l8:=$E18480; h8:=$E1859F; end;
  with cjkarr[67] do begin l:=8208; h:=8208; w:=cwA; l8:=$E28090; h8:=$E28090; end;
  with cjkarr[68] do begin l:=8211; h:=8214; w:=cwA; l8:=$E28093; h8:=$E28096; end;
  with cjkarr[69] do begin l:=8216; h:=8217; w:=cwA; l8:=$E28098; h8:=$E28099; end;
  with cjkarr[70] do begin l:=8220; h:=8221; w:=cwA; l8:=$E2809C; h8:=$E2809D; end;
  with cjkarr[71] do begin l:=8224; h:=8226; w:=cwA; l8:=$E280A0; h8:=$E280A2; end;
  with cjkarr[72] do begin l:=8228; h:=8231; w:=cwA; l8:=$E280A4; h8:=$E280A7; end;
  with cjkarr[73] do begin l:=8240; h:=8240; w:=cwA; l8:=$E280B0; h8:=$E280B0; end;
  with cjkarr[74] do begin l:=8242; h:=8243; w:=cwA; l8:=$E280B2; h8:=$E280B3; end;
  with cjkarr[75] do begin l:=8245; h:=8245; w:=cwA; l8:=$E280B5; h8:=$E280B5; end;
  with cjkarr[76] do begin l:=8251; h:=8251; w:=cwA; l8:=$E280BB; h8:=$E280BB; end;
  with cjkarr[77] do begin l:=8254; h:=8254; w:=cwA; l8:=$E280BE; h8:=$E280BE; end;
  with cjkarr[78] do begin l:=8308; h:=8308; w:=cwA; l8:=$E281B4; h8:=$E281B4; end;
  with cjkarr[79] do begin l:=8319; h:=8319; w:=cwA; l8:=$E281BF; h8:=$E281BF; end;
  with cjkarr[80] do begin l:=8321; h:=8324; w:=cwA; l8:=$E28281; h8:=$E28284; end;
  with cjkarr[81] do begin l:=8361; h:=8361; w:=cwH; l8:=$E282A9; h8:=$E282A9; end;
  with cjkarr[82] do begin l:=8364; h:=8364; w:=cwA; l8:=$E282AC; h8:=$E282AC; end;
  with cjkarr[83] do begin l:=8451; h:=8451; w:=cwA; l8:=$E28483; h8:=$E28483; end;
  with cjkarr[84] do begin l:=8453; h:=8453; w:=cwA; l8:=$E28485; h8:=$E28485; end;
  with cjkarr[85] do begin l:=8457; h:=8457; w:=cwA; l8:=$E28489; h8:=$E28489; end;
  with cjkarr[86] do begin l:=8467; h:=8467; w:=cwA; l8:=$E28493; h8:=$E28493; end;
  with cjkarr[87] do begin l:=8470; h:=8470; w:=cwA; l8:=$E28496; h8:=$E28496; end;
  with cjkarr[88] do begin l:=8481; h:=8482; w:=cwA; l8:=$E284A1; h8:=$E284A2; end;
  with cjkarr[89] do begin l:=8486; h:=8486; w:=cwA; l8:=$E284A6; h8:=$E284A6; end;
  with cjkarr[90] do begin l:=8491; h:=8491; w:=cwA; l8:=$E284AB; h8:=$E284AB; end;
  with cjkarr[91] do begin l:=8531; h:=8532; w:=cwA; l8:=$E28593; h8:=$E28594; end;
  with cjkarr[92] do begin l:=8539; h:=8542; w:=cwA; l8:=$E2859B; h8:=$E2859E; end;
  with cjkarr[93] do begin l:=8544; h:=8555; w:=cwA; l8:=$E285A0; h8:=$E285AB; end;
  with cjkarr[94] do begin l:=8560; h:=8569; w:=cwA; l8:=$E285B0; h8:=$E285B9; end;
  with cjkarr[95] do begin l:=8585; h:=8585; w:=cwA; l8:=$E28689; h8:=$E28689; end;
  with cjkarr[96] do begin l:=8592; h:=8601; w:=cwA; l8:=$E28690; h8:=$E28699; end;
  with cjkarr[97] do begin l:=8632; h:=8633; w:=cwA; l8:=$E286B8; h8:=$E286B9; end;
  with cjkarr[98] do begin l:=8658; h:=8658; w:=cwA; l8:=$E28792; h8:=$E28792; end;
  with cjkarr[99] do begin l:=8660; h:=8660; w:=cwA; l8:=$E28794; h8:=$E28794; end;
  with cjkarr[100] do begin l:=8679; h:=8679; w:=cwA; l8:=$E287A7; h8:=$E287A7; end;
  with cjkarr[101] do begin l:=8704; h:=8704; w:=cwA; l8:=$E28880; h8:=$E28880; end;
  with cjkarr[102] do begin l:=8706; h:=8707; w:=cwA; l8:=$E28882; h8:=$E28883; end;
  with cjkarr[103] do begin l:=8711; h:=8712; w:=cwA; l8:=$E28887; h8:=$E28888; end;
  with cjkarr[104] do begin l:=8715; h:=8715; w:=cwA; l8:=$E2888B; h8:=$E2888B; end;
  with cjkarr[105] do begin l:=8719; h:=8719; w:=cwA; l8:=$E2888F; h8:=$E2888F; end;
  with cjkarr[106] do begin l:=8721; h:=8721; w:=cwA; l8:=$E28891; h8:=$E28891; end;
  with cjkarr[107] do begin l:=8725; h:=8725; w:=cwA; l8:=$E28895; h8:=$E28895; end;
  with cjkarr[108] do begin l:=8730; h:=8730; w:=cwA; l8:=$E2889A; h8:=$E2889A; end;
  with cjkarr[109] do begin l:=8733; h:=8736; w:=cwA; l8:=$E2889D; h8:=$E288A0; end;
  with cjkarr[110] do begin l:=8739; h:=8739; w:=cwA; l8:=$E288A3; h8:=$E288A3; end;
  with cjkarr[111] do begin l:=8741; h:=8741; w:=cwA; l8:=$E288A5; h8:=$E288A5; end;
  with cjkarr[112] do begin l:=8743; h:=8748; w:=cwA; l8:=$E288A7; h8:=$E288AC; end;
  with cjkarr[113] do begin l:=8750; h:=8750; w:=cwA; l8:=$E288AE; h8:=$E288AE; end;
  with cjkarr[114] do begin l:=8756; h:=8759; w:=cwA; l8:=$E288B4; h8:=$E288B7; end;
  with cjkarr[115] do begin l:=8764; h:=8765; w:=cwA; l8:=$E288BC; h8:=$E288BD; end;
  with cjkarr[116] do begin l:=8776; h:=8776; w:=cwA; l8:=$E28988; h8:=$E28988; end;
  with cjkarr[117] do begin l:=8780; h:=8780; w:=cwA; l8:=$E2898C; h8:=$E2898C; end;
  with cjkarr[118] do begin l:=8786; h:=8786; w:=cwA; l8:=$E28992; h8:=$E28992; end;
  with cjkarr[119] do begin l:=8800; h:=8801; w:=cwA; l8:=$E289A0; h8:=$E289A1; end;
  with cjkarr[120] do begin l:=8804; h:=8807; w:=cwA; l8:=$E289A4; h8:=$E289A7; end;
  with cjkarr[121] do begin l:=8810; h:=8811; w:=cwA; l8:=$E289AA; h8:=$E289AB; end;
  with cjkarr[122] do begin l:=8814; h:=8815; w:=cwA; l8:=$E289AE; h8:=$E289AF; end;
  with cjkarr[123] do begin l:=8834; h:=8835; w:=cwA; l8:=$E28A82; h8:=$E28A83; end;
  with cjkarr[124] do begin l:=8838; h:=8839; w:=cwA; l8:=$E28A86; h8:=$E28A87; end;
  with cjkarr[125] do begin l:=8853; h:=8853; w:=cwA; l8:=$E28A95; h8:=$E28A95; end;
  with cjkarr[126] do begin l:=8857; h:=8857; w:=cwA; l8:=$E28A99; h8:=$E28A99; end;
  with cjkarr[127] do begin l:=8869; h:=8869; w:=cwA; l8:=$E28AA5; h8:=$E28AA5; end;
  with cjkarr[128] do begin l:=8895; h:=8895; w:=cwA; l8:=$E28ABF; h8:=$E28ABF; end;
  with cjkarr[129] do begin l:=8978; h:=8978; w:=cwA; l8:=$E28C92; h8:=$E28C92; end;
  with cjkarr[130] do begin l:=9001; h:=9002; w:=cwW; l8:=$E28CA9; h8:=$E28CAA; end;
  with cjkarr[131] do begin l:=9312; h:=9449; w:=cwA; l8:=$E291A0; h8:=$E293A9; end;
  with cjkarr[132] do begin l:=9451; h:=9547; w:=cwA; l8:=$E293AB; h8:=$E2958B; end;
  with cjkarr[133] do begin l:=9552; h:=9587; w:=cwA; l8:=$E29590; h8:=$E295B3; end;
  with cjkarr[134] do begin l:=9600; h:=9615; w:=cwA; l8:=$E29680; h8:=$E2968F; end;
  with cjkarr[135] do begin l:=9618; h:=9621; w:=cwA; l8:=$E29692; h8:=$E29695; end;
  with cjkarr[136] do begin l:=9632; h:=9633; w:=cwA; l8:=$E296A0; h8:=$E296A1; end;
  with cjkarr[137] do begin l:=9635; h:=9641; w:=cwA; l8:=$E296A3; h8:=$E296A9; end;
  with cjkarr[138] do begin l:=9650; h:=9651; w:=cwA; l8:=$E296B2; h8:=$E296B3; end;
  with cjkarr[139] do begin l:=9654; h:=9655; w:=cwA; l8:=$E296B6; h8:=$E296B7; end;
  with cjkarr[140] do begin l:=9660; h:=9661; w:=cwA; l8:=$E296BC; h8:=$E296BD; end;
  with cjkarr[141] do begin l:=9664; h:=9665; w:=cwA; l8:=$E29780; h8:=$E29781; end;
  with cjkarr[142] do begin l:=9670; h:=9672; w:=cwA; l8:=$E29786; h8:=$E29788; end;
  with cjkarr[143] do begin l:=9675; h:=9675; w:=cwA; l8:=$E2978B; h8:=$E2978B; end;
  with cjkarr[144] do begin l:=9678; h:=9681; w:=cwA; l8:=$E2978E; h8:=$E29791; end;
  with cjkarr[145] do begin l:=9698; h:=9701; w:=cwA; l8:=$E297A2; h8:=$E297A5; end;
  with cjkarr[146] do begin l:=9711; h:=9711; w:=cwA; l8:=$E297AF; h8:=$E297AF; end;
  with cjkarr[147] do begin l:=9733; h:=9734; w:=cwA; l8:=$E29885; h8:=$E29886; end;
  with cjkarr[148] do begin l:=9737; h:=9737; w:=cwA; l8:=$E29889; h8:=$E29889; end;
  with cjkarr[149] do begin l:=9742; h:=9743; w:=cwA; l8:=$E2988E; h8:=$E2988F; end;
  with cjkarr[150] do begin l:=9748; h:=9749; w:=cwA; l8:=$E29894; h8:=$E29895; end;
  with cjkarr[151] do begin l:=9756; h:=9756; w:=cwA; l8:=$E2989C; h8:=$E2989C; end;
  with cjkarr[152] do begin l:=9758; h:=9758; w:=cwA; l8:=$E2989E; h8:=$E2989E; end;
  with cjkarr[153] do begin l:=9792; h:=9792; w:=cwA; l8:=$E29980; h8:=$E29980; end;
  with cjkarr[154] do begin l:=9794; h:=9794; w:=cwA; l8:=$E29982; h8:=$E29982; end;
  with cjkarr[155] do begin l:=9824; h:=9825; w:=cwA; l8:=$E299A0; h8:=$E299A1; end;
  with cjkarr[156] do begin l:=9827; h:=9829; w:=cwA; l8:=$E299A3; h8:=$E299A5; end;
  with cjkarr[157] do begin l:=9831; h:=9834; w:=cwA; l8:=$E299A7; h8:=$E299AA; end;
  with cjkarr[158] do begin l:=9836; h:=9837; w:=cwA; l8:=$E299AC; h8:=$E299AD; end;
  with cjkarr[159] do begin l:=9839; h:=9839; w:=cwA; l8:=$E299AF; h8:=$E299AF; end;
  with cjkarr[160] do begin l:=9886; h:=9887; w:=cwA; l8:=$E29A9E; h8:=$E29A9F; end;
  with cjkarr[161] do begin l:=9918; h:=9919; w:=cwA; l8:=$E29ABE; h8:=$E29ABF; end;
  with cjkarr[162] do begin l:=9924; h:=9933; w:=cwA; l8:=$E29B84; h8:=$E29B8D; end;
  with cjkarr[163] do begin l:=9935; h:=9953; w:=cwA; l8:=$E29B8F; h8:=$E29BA1; end;
  with cjkarr[164] do begin l:=9955; h:=9955; w:=cwA; l8:=$E29BA3; h8:=$E29BA3; end;
  with cjkarr[165] do begin l:=9960; h:=9983; w:=cwA; l8:=$E29BA8; h8:=$E29BBF; end;
  with cjkarr[166] do begin l:=10045; h:=10045; w:=cwA; l8:=$E29CBD; h8:=$E29CBD; end;
  with cjkarr[167] do begin l:=10071; h:=10071; w:=cwA; l8:=$E29D97; h8:=$E29D97; end;
  with cjkarr[168] do begin l:=10102; h:=10111; w:=cwA; l8:=$E29DB6; h8:=$E29DBF; end;
  with cjkarr[169] do begin l:=10214; h:=10221; w:=cwNa; l8:=$E29FA6; h8:=$E29FAD; end;
  with cjkarr[170] do begin l:=10629; h:=10630; w:=cwNa; l8:=$E2A685; h8:=$E2A686; end;
  with cjkarr[171] do begin l:=11093; h:=11097; w:=cwA; l8:=$E2AD95; h8:=$E2AD99; end;
  with cjkarr[172] do begin l:=11904; h:=11929; w:=cwW; l8:=$E2BA80; h8:=$E2BA99; end;
  with cjkarr[173] do begin l:=11931; h:=12019; w:=cwW; l8:=$E2BA9B; h8:=$E2BBB3; end;
  with cjkarr[174] do begin l:=12032; h:=12245; w:=cwW; l8:=$E2BC80; h8:=$E2BF95; end;
  with cjkarr[175] do begin l:=12272; h:=12283; w:=cwW; l8:=$E2BFB0; h8:=$E2BFBB; end;
  with cjkarr[176] do begin l:=12288; h:=12288; w:=cwF; l8:=$E38080; h8:=$E38080; end;
  with cjkarr[177] do begin l:=12289; h:=12350; w:=cwW; l8:=$E38081; h8:=$E380BE; end;
  with cjkarr[178] do begin l:=12353; h:=12438; w:=cwW; l8:=$E38181; h8:=$E38296; end;
  with cjkarr[179] do begin l:=12441; h:=12543; w:=cwW; l8:=$E38299; h8:=$E383BF; end;
  with cjkarr[180] do begin l:=12549; h:=12589; w:=cwW; l8:=$E38485; h8:=$E384AD; end;
  with cjkarr[181] do begin l:=12593; h:=12686; w:=cwW; l8:=$E384B1; h8:=$E3868E; end;
  with cjkarr[182] do begin l:=12688; h:=12730; w:=cwW; l8:=$E38690; h8:=$E386BA; end;
  with cjkarr[183] do begin l:=12736; h:=12771; w:=cwW; l8:=$E38780; h8:=$E387A3; end;
  with cjkarr[184] do begin l:=12784; h:=12830; w:=cwW; l8:=$E387B0; h8:=$E3889E; end;
  with cjkarr[185] do begin l:=12832; h:=12871; w:=cwW; l8:=$E388A0; h8:=$E38987; end;
  with cjkarr[186] do begin l:=12872; h:=12879; w:=cwA; l8:=$E38988; h8:=$E3898F; end;
  with cjkarr[187] do begin l:=12880; h:=13054; w:=cwW; l8:=$E38990; h8:=$E38BBE; end;
  with cjkarr[188] do begin l:=13056; h:=19903; w:=cwW; l8:=$E38C80; h8:=$E4B6BF; end;
  with cjkarr[189] do begin l:=19968; h:=42124; w:=cwW; l8:=$E4B880; h8:=$EA928C; end;
  with cjkarr[190] do begin l:=42128; h:=42182; w:=cwW; l8:=$EA9290; h8:=$EA9386; end;
  with cjkarr[191] do begin l:=43360; h:=43388; w:=cwW; l8:=$EAA5A0; h8:=$EAA5BC; end;
  with cjkarr[192] do begin l:=44032; h:=55203; w:=cwW; l8:=$EAB080; h8:=$ED9EA3; end;
  with cjkarr[193] do begin l:=57344; h:=63743; w:=cwA; l8:=$EE8080; h8:=$EFA3BF; end;
  with cjkarr[194] do begin l:=63744; h:=64255; w:=cwW; l8:=$EFA480; h8:=$EFABBF; end;
  with cjkarr[195] do begin l:=65024; h:=65039; w:=cwA; l8:=$EFB880; h8:=$EFB88F; end;
  with cjkarr[196] do begin l:=65040; h:=65049; w:=cwW; l8:=$EFB890; h8:=$EFB899; end;
  with cjkarr[197] do begin l:=65072; h:=65106; w:=cwW; l8:=$EFB8B0; h8:=$EFB992; end;
  with cjkarr[198] do begin l:=65108; h:=65126; w:=cwW; l8:=$EFB994; h8:=$EFB9A6; end;
  with cjkarr[199] do begin l:=65128; h:=65131; w:=cwW; l8:=$EFB9A8; h8:=$EFB9AB; end;
  with cjkarr[200] do begin l:=65281; h:=65376; w:=cwF; l8:=$EFBC81; h8:=$EFBDA0; end;
  with cjkarr[201] do begin l:=65377; h:=65470; w:=cwH; l8:=$EFBDA1; h8:=$EFBEBE; end;
  with cjkarr[202] do begin l:=65474; h:=65479; w:=cwH; l8:=$EFBF82; h8:=$EFBF87; end;
  with cjkarr[203] do begin l:=65482; h:=65487; w:=cwH; l8:=$EFBF8A; h8:=$EFBF8F; end;
  with cjkarr[204] do begin l:=65490; h:=65495; w:=cwH; l8:=$EFBF92; h8:=$EFBF97; end;
  with cjkarr[205] do begin l:=65498; h:=65500; w:=cwH; l8:=$EFBF9A; h8:=$EFBF9C; end;
  with cjkarr[206] do begin l:=65504; h:=65510; w:=cwF; l8:=$EFBFA0; h8:=$EFBFA6; end;
  with cjkarr[207] do begin l:=65512; h:=65518; w:=cwH; l8:=$EFBFA8; h8:=$EFBFAE; end;
  with cjkarr[208] do begin l:=65533; h:=65533; w:=cwA; l8:=$EFBFBD; h8:=$EFBFBD; end;
  with cjkarr[209] do begin l:=110592; h:=110593; w:=cwW; l8:=$F09B8080; h8:=$F09B8081; end;
  with cjkarr[210] do begin l:=127232; h:=127242; w:=cwA; l8:=$F09F8480; h8:=$F09F848A; end;
  with cjkarr[211] do begin l:=127248; h:=127277; w:=cwA; l8:=$F09F8490; h8:=$F09F84AD; end;
  with cjkarr[212] do begin l:=127280; h:=127337; w:=cwA; l8:=$F09F84B0; h8:=$F09F85A9; end;
  with cjkarr[213] do begin l:=127344; h:=127386; w:=cwA; l8:=$F09F85B0; h8:=$F09F869A; end;
  with cjkarr[214] do begin l:=127488; h:=127490; w:=cwW; l8:=$F09F8880; h8:=$F09F8882; end;
  with cjkarr[215] do begin l:=127504; h:=127546; w:=cwW; l8:=$F09F8890; h8:=$F09F88BA; end;
  with cjkarr[216] do begin l:=127552; h:=127560; w:=cwW; l8:=$F09F8980; h8:=$F09F8988; end;
  with cjkarr[217] do begin l:=127568; h:=127569; w:=cwW; l8:=$F09F8990; h8:=$F09F8991; end;
  with cjkarr[218] do begin l:=131072; h:=196605; w:=cwW; l8:=$F0A08080; h8:=$F0AFBFBD; end;
  with cjkarr[219] do begin l:=196608; h:=262141; w:=cwW; l8:=$F0B08080; h8:=$F0BFBFBD; end;
  with cjkarr[220] do begin l:=917760; h:=917999; w:=cwA; l8:=$F3A08480; h8:=$F3A087AF; end;
  with cjkarr[221] do begin l:=983040; h:=1048573; w:=cwA; l8:=$F3B08080; h8:=$F3BFBFBD; end;
  // end of automatically generated part
end;




function GetCJKWidth(u: Integer; defaultWidth: TCharWidth = cwN): TCharWidth;
var
  b: Integer;
  e: Integer;
  i: Integer;
begin
  if length(cjkarr)=0 then InitCJKWidth;

  // simple binary search
  b  := 0;
  e  := length(cjkarr)-1;
  Result := defaultWidth;

  while (b <= e) do
  begin
    i := (b + e) div 2;
    if (u>=cjkarr[i].l) and (u<=cjkarr[i].h) then begin
      Result := cjkarr[i].w;
      Break;
    end else if cjkarr[i].l > u then
      e := i - 1
    else
      b := i + 1;
  end;
end;

function GetCJKWidth(utf8: PChar; charLen: Integer; defWidth: TCharWidth): TCharWidth; forward;

function GetCJKWidth(utf8: PChar; defWidth: TCharWidth): TCharWidth;
var
  l : integer;
const
  len1 = $01 shl 7;  mask1 = $00;
  len2 = $07 shl 5;  mask2 = $C0;
  len3 = $0F shl 4;  mask3 = $E0;
  len4 = $1F shl 4;  mask4 = $F0;
  len5 = $3F shl 4;  mask5 = $F8;
  len6 = $7F shl 4;  mask6 = $FC;
begin
  if not AssigneD(utf8) then begin
    Result:=defWidth;
    Exit;
  end;
  l:=0;
  if byte(utf8^) and len1 = mask1 then l:=1
  else if byte(utf8^) and len2 = mask2 then l:=2
  else if byte(utf8^) and len3 = mask3 then l:=3
  else if byte(utf8^) and len4 = mask4 then l:=4
  else if byte(utf8^) and len5 = mask5 then l:=5
  else if byte(utf8^) and len6 = mask6 then l:=6;
  if l=0 then Result:=defWidth
  else Result:=GetCJKWidth(utf8, l, defWidth)
end;

function GetCJKWidth(utf8: PChar; charLen: Integer; defWidth: TCharWidth): TCharWidth;
var
  c : Int64;
  pb : PByteArray;
  b: Integer;
  e: Integer;
  i: Integer;
begin
  {$IFDEF ENDIAN_LITTLE}
  case charLen of
    1: c:=byte(utf8^);
    2: c:=SwapEndian(PWord(utf8)^);
    3: begin
      pb:=PByteArray(utf8);
      c:=(pb^[0] shl 16) or (pb^[1] shl 8) or (pb^[2]);
    end;
    4: c:=SwapEndian(PLongWord(utf8)^);
    5: begin
      pb:=PByteArray(utf8);
      c:=(pb^[0] shl 32) or (pb^[1] shl 24) or (pb^[2] shl 16) or (pb^[3] shl 8) or (pb^[4]);
    end;
    6: begin
      pb:=PByteArray(utf8);
      c:=(pb^[0] shl 40) or (pb^[1] shl 32) or (pb^[2] shl 24) or (pb^[3] shl 16) or (pb^[4] shl 8) or (pb^[5]);
    end;
  else
    Result:=defWidth;
    Exit;
  end;
  {$ELSE}
  c:=0;
  move(utf8^, c, charLen);
  {$ENDIF}

  // simple binary search
  b  := 0;
  e  := length(cjkarr)-1;
  Result := defWidth;

  while (b <= e) do
  begin
    i := (b + e) div 2;
    if (c>=cjkarr[i].l8) and (c<=cjkarr[i].h8) then begin
      Result := cjkarr[i].w;
      Break;
    end else if cjkarr[i].l8 > c then
      e := i - 1
    else
      b := i + 1;
  end;
end;

{$ENDIF}


{ SynEditTextDoubleWidthChars }

procedure SynEditStringDoubleWidthChars.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
var
  i: Integer;
begin
  inherited DoGetPhysicalCharWidths(Line, LineLen, Index, PWidths);
  if not IsUtf8 then
    exit;

  dec(Line);
  dec(PWidths);

  {$IFDEF SynForceDoubeWidthHack}
  {$IF FPC_FULLVERSION>=20701}
  if (DefaultSystemCodePage = 932) {Japanese}
  {$ELSE}
  if (GetACP = 932) {Japanese}
  {$ENDIF}
  then begin
    for i := 0 to LineLen - 1 do begin
      inc(Line);
      inc(PWidths);
      if PWidths^ = 0 then continue;

      case GetCJKWidth(Line, cwN) of
        cwN, cwH, cwNa: PWidths^ := 1;
        cwA, cwW, cwF:  PWidths^ := 2;
      end;

      (*
      PWidths^:=2;
      case Line^ of
        #$01..#$7F: PWidths^ := 1;
        #$80..#$BF: PWidths^ := 0;
        #$EF: begin
          if (Line[1] = #$bd) and (Line[2] in [#$A1..#$bf]) then PWidths^ := 1;
          if (Line[1] = #$be) and (Line[2] in [#$80..#$9f]) then PWidths^ := 1;
        end;
      end;
      *)
    end;
    exit;
  end;
  {$ENDIF}

  for i := 0 to LineLen - 1 do begin
    inc(Line);
    inc(PWidths);
    if Line^ < #$e1 then continue;
    if PWidths^ = 0 then continue;
    case Line^ of
      #$e1:
        case Line[1] of
          #$84:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$85:
            if (Line[2] <= #$9f) then PWidths^ := 2;
        end;
      #$e2:
        case Line[1] of
          #$8c:
            if (Line[2] = #$a9) or (Line[2] = #$aa) then PWidths^ := 2;
          #$ba:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$bb..#$ff:
            PWidths^ := 2;
        end;
      #$e3:
        case Line[1] of
          #$80:
            if (Line[2] >= #$80) and (Line[2] <= #$be) then PWidths^ := 2;
          #$81:
            if (Line[2] >= #$81) then PWidths^ := 2;
          #$82..#$8e:
            PWidths^ := 2;
          #$8f:
            if (Line[2] <= #$bf) then PWidths^ := 2;
          #$90:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$91..#$FF:
            PWidths^ := 2;
        end;
      #$e4:
        case Line[1] of
          #$00..#$b5:
            PWidths^ := 2;
          #$b6:
            if (Line[2] <= #$b5) then PWidths^ := 2;
          #$b8:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$b9..#$ff:
            PWidths^ := 2;
        end;
      #$e5..#$e8:
        PWidths^ := 2;
      #$e9:
        if (Line[1] <= #$bf) or (Line[2] <= #$83) then PWidths^ := 2;
      #$ea:
        case Line[1] of
          #$80, #$b0:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$81..#$92, #$b1..#$ff:
            PWidths^ := 2;
          #$93:
            if (Line[2] <= #$86) then PWidths^ := 2;
        end;
      #$eb..#$ec:
        PWidths^ := 2;
      #$ed:
        if (Line[1] <= #$9e) or (Line[2] <= #$a3) then PWidths^ := 2;

      #$ef:
        case Line[1] of
          #$a4:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$a5..#$aa:
            PWidths^ := 2;
          #$ab:
            if (Line[2] <= #$99) then PWidths^ := 2;
          #$b8:
            if (Line[2] in [#$90..#$99,#$b0..#$ff]) then PWidths^ := 2;
          #$b9:
            if (Line[2] <= #$ab) then PWidths^ := 2;
          #$bc:
            if (Line[2] >= #$81) then PWidths^ := 2;
          #$bd:
            if (Line[2] <= #$a0) then PWidths^ := 2;
          #$bf:
            if (Line[2] >= #$a0) and (Line[2] <= #$a6) then PWidths^ := 2;
        end;
      #$f0:
        case Line[1] of
          #$a0, #$b0:
            case Line[2] of
              #$80:
                if (Line[3] >= #$80) then PWidths^ := 2;
              #$81..#$ff:
                PWidths^ := 2;
            end;
          #$a1..#$ae, #$b1..#$be:
            PWidths^ := 2;
          #$af, #$bf:
            case Line[2] of
              #$00..#$be:
                PWidths^ := 2;
              #$bf:
                if (Line[3] <= #$bd) then PWidths^ := 2;
            end;
        end
    end;
  end
end;

(* Ranges that are FullWidth char

 1100  e1 84 80  ..  115F  e1 85 9f
 2329  e2 8c a9  ..  232A  e2 8c aa
 2E80  e2 ba 80  ..  303E  e3 80 be
 3041  e3 81 81  ..  33FF  e3 8f bf
 3400  e3 90 80  ..  4DB5  e4 b6 b5
 4E00  e4 b8 80  ..  9FC3  e9 bf 83
 A000  ea 80 80  ..  A4C6  ea 93 86
 AC00  ea b0 80  ..  D7A3  ed 9e a3
 F900  ef a4 80  ..  FAD9  ef ab 99
 FE10  ef b8 90  ..  FE19  ef b8 99
 FE30  ef b8 b0  ..  FE6B  ef b9 ab
 FF01  ef bc 81  ..  FF60  ef bd a0
 FFE0  ef bf a0  ..  FFE6  ef bf a6
20000  f0 a0 80 80  .. 2FFFD f0 af bf bd
30000  f0 b0 80 80  .. 3FFFD f0 bf bf bd

*)

{$IFDEF SynForceDoubeWidthHack}
initialization
  InitCJKWidth;
{$ENDIF}

end.

