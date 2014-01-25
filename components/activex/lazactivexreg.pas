unit lazactivexreg;

{ ActiveX component registration unit.

  Copyright (C) 2011 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

interface

uses
  activexcontainer
  {$ifndef wince}
  ,ImportTypelib
  {$endif wince}
  ;

procedure Register;

implementation

uses Classes, LResources, MenuIntf, LCLIntf, activexstrconsts;


procedure Register; 
begin
  {$ifndef wince}
  RegisterIDEMenuCommand(itmSecondaryTools, 'ImportTL', axImportTypeLibraryMenu, nil
    , @ImpTypeLib);
  {$endif wince}
  RegisterComponents('ActiveX', [TActiveXContainer]);
end;

initialization
LazarusResources.Add('TActiveXContainer','PNG',[
  #137'PNG'#13#10#26#10#0#0#0#13'IHDR'#0#0#0#24#0#0#0#24#8#6#0#0#0#224'w='#248#0
  +#0#0#1'sRGB'#0#174#206#28#233#0#0#0#6'bKGD'#0#0#0#0#0#0#249'C'#187#127#0#0#0
  +#9'pHYs'#0#0#11#19#0#0#11#19#1#0#154#156#24#0#0#0#7'tIME'#7#219#12#29#15''''
  +#14#229#143#252'P'#0#0#0#25'tEXtComment'#0'Created with GIMPW'#129#14#23#0#0
  +#4')IDATH'#199#181'U{L[e'#20#255#221'{K'#233#198#128'B-'#143#14'6'#137#136
  +#145#133'm'#248#136#178#224#20#31#141'!Y'#156#203'6'#19#163#152#12'uc2'#147
  +#205'L'#137'qf'#3'u'#137#25'f'#226#8#198'E'#205'fDE'#156'l#'#153#147#9's'#15
  +#31'A'#172#186'!'#8#148#142'G'#25#184#130#164#133#246#182#183#189'?'#255#168
  +#180'tc'#19'f<'#201#151#220#243#251#206#247#157#243';'#231#220#243#129#215'!'
  +#129#128#159#173'?[X'#177'{'#23#11#205#133'4'#153'R'#168#145't'#20'5Zfd.fI'
  +#201's'#236#183#245#146'$'#5#146#196','#197'91'#129#143#15#30#196#254#253'5'
  +#24#232#27#196#205#183','#193'M'#153#153'HH'#140#3'D'#21#23#251#134#241#195
  +#175'm'#176#219#250#144#148'l'#196#201#211#223#2#179#137#216#227#245#176#186
  +#178#154#249#249#249',+{'#137#223#159'=EY'#150'g'#180'U'#252#10#27#27#191'd'
  +#154')'#141'E'#143#23#241'_'#29#252'x'#230#12#183'n-a'#227#209#6#250#188#222
  +'Y'#167#209#242#203'o\'#190#252#142#171#167'H'#9'(8'#252#217#17'$.'#212#163
  +'`'#229#253#16#4#1's'#18#2'e'#175#150#205#156'"'#167'k'#130#13#13'G'#216'?'
  +#216#207#255'"'#173#167'N'#135#25#144#132#127'2'#128'q'#223'_'#240#184'ehU'
  +#21'Z'#173#17'q'#198'hH'#146#8#1#151'1'#8'('#145#186' '#0#162#6#160#10#168
  +#129'iL'#168#210#218#221#206#141#165'E\'#176#192#200' '#185#169'%'#241#6'S'#6
  +#183'm'#127#145#206'1'#231#180'>U'#232#223#187#134'|'#6#225#245#154#153'Td'
  +#178'n['#24'+'#141'%l'#253'Vf'#230'f]v'#241#149'k'#195#19'OF'#208#247#14'v'
  +#146'['#226#167'9'#153'O'#182#188'On'#137#253'G'#215#145'-'#31'Q|go'#5'z,]A'
  +#150#0#30'-|'#4#159#31':'#140'=o'#238#137#200#192#209#227'_'#195#237'q'#134
  +'t'#173')'#11#184'm'#245'4'#11'7P['#12#200#174#160#250#192'v'#224#158'u'#16#6
  +'G'''#233#153#184#4#199#208#5't'#245#158'G'#140'a!d'#167#11'#]N'#188#242'z)<'
  +#158#160'}BB'#2'::~GrrJ'#248'Nk;'#240'V>'#160#140'G'#214'c'#201'z'#224#249'O'
  +#0'A'#132'&%>'#26'g[;PUY'#141#150's'#205#24#27'v'#207#216'u'#138#162#192#225
  +#24#141'tpc'#6#160#215#3#151#166'9'#136#210#1#143#149#3#130#8#0#16'kk'#15#161
  +'p'#205'Z|'#209#212#136#177'a7$'#9#184'7o'#25'J7o'#130#30#209#161's'#170#170
  +#194#231#243'Ez'#253#230#3'`l '#18#243#251#0#155'%'#172#167#153#210'#'#138'Y'
  +#252't1='#147#19#28#31#178'S'#132#24#194#227#227#227'i'#179#217#194'U'#254
  +#238#0'Y'#162#11#22#244'Y)'#178#163'v'#230#146#178#135'$)'#142#143'8"'#2#136
  +'I4`'#200'1'#138#154#154'w'#161'B'#13#225#162'(A'#21#162'A'#18#242#248#16#240
  +#225'S'#128'_'#14'n'#174#170#2#140#139#195#151#216'-'#192'O'#245#193#239#149
  +'y'#183'G0'#144'$'#137#177#6#3#5'Q$'#180#218#16#30#27#23#199#246#206'n*'#178
  +#151#220#183'*'#28#237#190#135#200'@'#128'<'#240'r$'#139#221#185#164#215'E'
  +#177#170#242'=dg'#166#135#127#208'@'#0#174#137'Q'#188#176#185#8'wg'#223#25
  +#194'eY'#134'u'#168#31#154#230'7'#128#243#199#130#160'!'#21'X'#251'6 '#138'@'
  +#193#6'`'#158'1'#204#162#215#2#180'5B IYQ@'#191#2#18'P'#252'^Di'#162'0O7'#31
  +#170#26'L'#145'J'#194#229'r# '#1#198#216#24'`j>N'#141#135'P'''#248#195'{@'
  +#176#147#186';;'#175'k'#144'MNN'#178#233#228#9#246#246't]'#211'N2-J'#219#153
  +#179'4'#7#186'h'#221#172#166#240#232#184#3#159#214#213#193#250#135#21'f'#243
  +#131'0'#26'S'#174'}'#224#216#241#175#184'bE'#30'-mmW'#141#194#241#167#131#245
  +#245#245#220'X'#178#137';v'#236#226#160'}`'#214'L5'#15#155#205#232#232'8'#135
  +#251#10#10#144#150#190#8'9Ko'#133'A'#159#10#149'n'#12#219'G`'#187#208#15#189
  +#222#128'u'#235'W'#163#188#162#28'I'#6#227#156#222#157#208'{`'#191'hG'#211
  +#137'ft'#247#244'@U|0&&#;+'#11#203#238#202'EjR*0'#199#7#237#10#7#255#151#252
  +#13#208#236#3#252#184#199'u'#249#0#0#0#0'IEND'#174'B`'#130
]);

end.
