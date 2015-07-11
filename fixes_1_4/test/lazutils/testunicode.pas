program TestUnicode;

{$mode objfpc}{$H+}

uses
  sysutils, lazutf8, lazutf16;

procedure WriteStringHex(Str: utf8string);
var
  StrOut: utf8string;
  i: Integer;
begin
  StrOut := '';
  for i := 1 to Length(Str) do
  begin
    StrOut := StrOut + IntToHex(Byte(Str[i]), 2) + ' ';
  end;
  Write(StrOut);
end;

procedure AssertStringOperation(AMsg, AStr1, AStr2, AStrExpected2: utf8string);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if UTF8CompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
    Write('Got      Len=', Length(AStr2), ' Str=');
    WriteStringHex(AStr2);
    WriteLn('');
    Write('Expected Len=', Length(AStrExpected2), ' Str=');
    WriteStringHex(AStrExpected2);
    WriteLn();
    Write('Orig     Len=', Length(AStr1), ' Str=');
    WriteStringHex(AStr1);
    WriteLn('');
  end;
  WriteLn();
end;

procedure AssertStringOperationUTF8UpperCase(AMsg, ALocale, AStr1, AStrExpected2: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8UpperCase(AStr1, ALocale), AStrExpected2);
end;

procedure AssertStringOperationUTF8LowerCase(AMsg, ALocale, AStr1, AStrExpected2: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8LowerCase(AStr1, ALocale), AStrExpected2);
//  AssertStringOperation('2'+AMsg, AStr1, UTF8LowerCase2(AStr1, ALocale), AStrExpected2);
//  AssertStringOperation('M'+AMsg, AStr1, UTF8LowerCaseMattias(AStr1), AStrExpected2);
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

procedure TestUTF8UpperCase;
var
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
  i: Integer;
begin
  // ASCII
  AssertStringOperationUTF8UpperCase('ASCII UTF8UpperCase', '', 'abcdefghijklmnopqrstuwvxyz', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  // Latin
  AssertStringOperationUTF8UpperCase('Portuguese UTF8UpperCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'Ç/Ç Ã/Ã Õ/Õ Á/Á É/É Í/Í Ó/Ó Ú/Ú Ü/Ü À/À Â/Â Ê/Ê Î/Î Ô/Ô Û/Û');
  AssertStringOperationUTF8UpperCase('French UTF8UpperCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'À/À Â/Â Æ Ç/Ç É/É È/È Ê/Ê Ë/Ë Î/Î Ï/Ï Ô/Ô Œ Ù/Ù Û/Û Ü/Ü Ÿ/Ÿ');
  AssertStringOperationUTF8UpperCase('Polish UTF8UpperCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertStringOperationUTF8UpperCase('Polish UTF8UpperCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertStringOperationUTF8UpperCase('German UTF8UpperCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'Ä/Ä,Ö/Ö,Ü/Ü,SS');
  // Turkish
  AssertStringOperationUTF8UpperCase('Turkish UTF8UpperCase 1', 'tr', 'abcçdefgğhııijklmnoöprsştuüvyz', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  AssertStringOperationUTF8UpperCase('Turkish UTF8UpperCase 2', 'tr', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  // Cyrillic
  AssertStringOperationUTF8UpperCase('Russian UTF8UpperCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  AssertStringOperationUTF8UpperCase('Russian UTF8UpperCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  // Unicode table
  AssertStringOperationUTF8UpperCase('Latin 00C0 UTF8UpperCase', '', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ');
  AssertStringOperationUTF8UpperCase('Latin 00D0 UTF8UpperCase', '', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞSS');
  AssertStringOperationUTF8UpperCase('Latin 00E0 UTF8UpperCase', '', 'àáâãäåæçèéêëìíîï', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ');
  AssertStringOperationUTF8UpperCase('Latin 00F0 UTF8UpperCase', '', 'ðñòóôõö÷øùúûüýþÿ', 'ÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞŸ');
  AssertStringOperationUTF8UpperCase('Latin 0100 UTF8UpperCase', '', 'ĀāĂăĄąĆćĈĉĊċČčĎď', 'ĀĀĂĂĄĄĆĆĈĈĊĊČČĎĎ');
  AssertStringOperationUTF8UpperCase('Latin 0110 UTF8UpperCase', '', 'ĐđĒēĔĕĖėĘęĚěĜĝĞğ', 'ĐĐĒĒĔĔĖĖĘĘĚĚĜĜĞĞ');
  AssertStringOperationUTF8UpperCase('Latin 0120 UTF8UpperCase', '', 'ĠġĢģĤĥĦħĨĩĪīĬĭĮį', 'ĠĠĢĢĤĤĦĦĨĨĪĪĬĬĮĮ');
  AssertStringOperationUTF8UpperCase('Latin 0130 UTF8UpperCase', '', 'İıĲĳĴĵĶķĸĹĺĻļĽľĿ', 'İIĲĲĴĴĶĶĸĹĹĻĻĽĽĿ');
  AssertStringOperationUTF8UpperCase('Latin 0140 UTF8UpperCase', '', 'ŀŁłŃńŅņŇňŉŊŋŌōŎŏ', 'ĿŁŁŃŃŅŅŇŇŉŊŊŌŌŎŎ');
  AssertStringOperationUTF8UpperCase('Latin 0150 UTF8UpperCase', '', 'ŐőŒœŔŕŖŗŘřŚśŜŝŞş', 'ŐŐŒŒŔŔŖŖŘŘŚŚŜŜŞŞ');
  AssertStringOperationUTF8UpperCase('Latin 0160 UTF8UpperCase', '', 'ŠšŢţŤťŦŧŨũŪūŬŭŮů', 'ŠŠŢŢŤŤŦŦŨŨŪŪŬŬŮŮ');
  AssertStringOperationUTF8UpperCase('Latin 0170 UTF8UpperCase', '', 'ŰűŲųŴŵŶŷŸŹźŻżŽžſ', 'ŰŰŲŲŴŴŶŶŸŹŹŻŻŽŽS');
  AssertStringOperationUTF8UpperCase('Latin 0180 UTF8UpperCase', '', 'ƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏ', 'ɃƁƂƂƄƄƆƇƇƉƊƋƋƍƎƏ');
  AssertStringOperationUTF8UpperCase('Latin 0190 UTF8UpperCase', '', 'ƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟ', 'ƐƑƑƓƔǶƖƗƘƘȽƛƜƝȠƟ');
  AssertStringOperationUTF8UpperCase('Latin 01A0 UTF8UpperCase', '', 'ƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯ', 'ƠƠƢƢƤƤƦƧƧƩƪƫƬƬƮƯ');
  AssertStringOperationUTF8UpperCase('Latin 01B0 UTF8UpperCase', '', 'ưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿ', 'ƯƱƲƳƳƵƵƷƸƸƺƻƼƼƾǷ');
  AssertStringOperationUTF8UpperCase('Latin 01C0 UTF8UpperCase', '', 'ǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏ', 'ǀǁǂǃǄǄǄǇǇǇǊǊǊǍǍǏ');
  AssertStringOperationUTF8UpperCase('Latin 01D0 UTF8UpperCase', '', 'ǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ', 'ǏǑǑǓǓǕǕǗǗǙǙǛǛƎǞǞ');
  AssertStringOperationUTF8UpperCase('Latin 01E0 UTF8UpperCase', '', 'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯ', 'ǠǠǢǢǤǤǦǦǨǨǪǪǬǬǮǮ');
  AssertStringOperationUTF8UpperCase('Latin 01F0 UTF8UpperCase', '', 'ǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿ', 'ǰǱǱǱǴǴǶǷǸǸǺǺǼǼǾǾ');
  AssertStringOperationUTF8UpperCase('Latin 0200 UTF8UpperCase', '', 'ȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏ', 'ȀȀȂȂȄȄȆȆȈȈȊȊȌȌȎȎ');
  AssertStringOperationUTF8UpperCase('Latin 0210 UTF8UpperCase', '', 'ȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟ', 'ȐȐȒȒȔȔȖȖȘȘȚȚȜȜȞȞ');
  AssertStringOperationUTF8UpperCase('Latin 0220 UTF8UpperCase', '', 'ȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯ', 'ȠȡȢȢȤȤȦȦȨȨȪȪȬȬȮȮ');
  AssertStringOperationUTF8UpperCase('Latin 0230 UTF8UpperCase', '', 'ȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿ', 'ȰȰȲȲȴȵȶȷȸȹȺȻȻȽȾⱾ');
  AssertStringOperationUTF8UpperCase('Latin 0240 UTF8UpperCase', '', 'ɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏ', 'ⱿɁɁɃɄɅɆɆɈɈɊɊɌɌɎɎ');
  AssertStringOperationUTF8UpperCase('Latin 0250 UTF8UpperCase', '', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ', 'ⱯⱭⱰƁƆɕƉƊɘƏɚƐɜɝɞɟ');
  AssertStringOperationUTF8UpperCase('Latin 0260 UTF8UpperCase', '', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ', 'ƓɡɢƔɤꞍɦɧƗƖɪⱢɬɭɮƜ');
  AssertStringOperationUTF8UpperCase('Latin 0270 UTF8UpperCase', '', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ', 'ɰⱮƝɳɴƟɶɷɸɹɺɻɼⱤɾɿ');
  AssertStringOperationUTF8UpperCase('Latin 0280 UTF8UpperCase', '', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ', 'ƦʁʂƩʄʅʆʇƮɄƱƲɅʍʎʏ');
  AssertStringOperationUTF8UpperCase('Latin 0290 UTF8UpperCase', '', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ', 'ʐʑƷʓʔʕʖʗʘʙʚʛʜʝʞʟ');
  AssertStringOperationUTF8UpperCase('Latin 02A0 UTF8UpperCase', '', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ');
  AssertStringOperationUTF8UpperCase('Unicode 0380 UTF8UpperCase', '', '΄΅Ά·ΈΉΊΌΎΏ', '΄΅Ά·ΈΉΊΌΎΏ');
  AssertStringOperationUTF8UpperCase('Unicode 0390 UTF8UpperCase', '', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ');
  AssertStringOperationUTF8UpperCase('Unicode 03A0 UTF8UpperCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'ΠΡΣΤΥΦΧΨΩΪΫΆΈΉΊ');
  AssertStringOperationUTF8UpperCase('Unicode 03B0 UTF8UpperCase', '', 'ΰαβγδεζηθικλμνξο', 'ΰΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ');
  AssertStringOperationUTF8UpperCase('Unicode 03C0 UTF8UpperCase', '', 'πρςστυφχψωϊϋόύώϏ', 'ΠΡΣΣΤΥΦΧΨΩΪΫΌΎΏϏ');
  AssertStringOperationUTF8UpperCase('Unicode 03D0 UTF8UpperCase', '', 'ϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟ', 'ΒΘϒϓϔΦΠϏϘϘϚϚϜϜϞϞ');
  AssertStringOperationUTF8UpperCase('Unicode 03E0 UTF8UpperCase', '', 'ϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯ', 'ϠϠϢϢϤϤϦϦϨϨϪϪϬϬϮϮ');
  AssertStringOperationUTF8UpperCase('Unicode 03F0 UTF8UpperCase', '', 'ϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿ', 'ΚΡϹϳϴΕ϶ϷϷϹϺϺϼϽϾϿ');
  AssertStringOperationUTF8UpperCase('Unicode 0400 UTF8UpperCase', '', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ');
  AssertStringOperationUTF8UpperCase('Unicode 0410 UTF8UpperCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertStringOperationUTF8UpperCase('Unicode 0420 UTF8UpperCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'РСТУФХЦЧШЩЪЫЬЭЮЯ');
  AssertStringOperationUTF8UpperCase('Unicode 0430 UTF8UpperCase', '', 'абвгдежзийклмноп', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertStringOperationUTF8UpperCase('Unicode 0440 UTF8UpperCase', '', 'рстуфхцчшщъыьэюя', 'РСТУФХЦЧШЩЪЫЬЭЮЯ');
  AssertStringOperationUTF8UpperCase('Unicode 0450 UTF8UpperCase', '', 'ѐёђѓєѕіїјљњћќѝўџ', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ');

  // What shouldnt change
  AssertStringOperationUTF8UpperCase('Chinese UTF8UpperCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');

  // Performance test
  lStartTime := Now;
  for i := 0 to 9999 do
  begin
    Str := UTF8UpperCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
    Str := Str + UTF8UpperCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
    Str := Str + UTF8UpperCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  end;
  lTimeDiff := Now - lStartTime;
  WriteLn('UpperCase Performance test took: ', DateTimeToMilliseconds(lTimeDiff), ' ms');
end;

procedure TestUTF8LowerCase;
var
  k, j, i: Integer;
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
const
  TimerLoop = 5999999;
begin
  // ASCII
  AssertStringOperationUTF8LowerCase('ASCII UTF8LowerCase', '', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ', 'abcdefghijklmnopqrstuwvxyz');
  // Latin
  AssertStringOperationUTF8LowerCase('Portuguese UTF8LowerCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'ç/ç ã/ã õ/õ á/á é/é í/í ó/ó ú/ú ü/ü à/à â/â ê/ê î/î ô/ô û/û');
  AssertStringOperationUTF8LowerCase('French UTF8LowerCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'à/à â/â æ ç/ç é/é è/è ê/ê ë/ë î/î ï/ï ô/ô œ ù/ù û/û ü/ü ÿ/ÿ');
  AssertStringOperationUTF8LowerCase('Polish UTF8LowerCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('Polish UTF8LowerCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('German UTF8LowerCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'ä/ä,ö/ö,ü/ü,ß');
  // Unicode table
  AssertStringOperationUTF8LowerCase('Latin 00C0 UTF8LowerCase', '', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ', 'àáâãäåæçèéêëìíîï');
  AssertStringOperationUTF8LowerCase('Latin 00D0 UTF8LowerCase', '', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß', 'ðñòóôõö×øùúûüýþß');
  AssertStringOperationUTF8LowerCase('Latin 00E0 UTF8LowerCase', '', 'àáâãäåæçèéêëìíîï', 'àáâãäåæçèéêëìíîï');
  AssertStringOperationUTF8LowerCase('Latin 00F0 UTF8LowerCase', '', 'ðñòóôõö÷øùúûüýþÿ', 'ðñòóôõö÷øùúûüýþÿ');
  AssertStringOperationUTF8LowerCase('Latin 0100 UTF8LowerCase', '', 'Āā Ăă Ąą Ćć Ĉĉ Ċċ Čč Ďď', 'āā ăă ąą ćć ĉĉ ċċ čč ďď');
  AssertStringOperationUTF8LowerCase('Latin 0110 UTF8LowerCase', '', 'ĐđĒēĔĕĖėĘęĚěĜĝĞğ', 'đđēēĕĕėėęęěěĝĝğğ');
  AssertStringOperationUTF8LowerCase('Latin 0120 UTF8LowerCase', '', 'ĠġĢģĤĥĦħĨĩĪīĬĭĮį', 'ġġģģĥĥħħĩĩīīĭĭįį');
  AssertStringOperationUTF8LowerCase('Latin 0130 UTF8LowerCase', '', 'İıĲĳĴĵĶķĸĹĺĻļĽľĿ', 'iıĳĳĵĵķķĸĺĺļļľľŀ');
  AssertStringOperationUTF8LowerCase('Latin 0140 UTF8LowerCase', '', 'ŀŁłŃńŅņŇňŉŊŋŌōŎŏ', 'ŀłłńńņņňňŉŋŋōōŏŏ');
  AssertStringOperationUTF8LowerCase('Latin 0150 UTF8LowerCase', '', 'ŐőŒœŔŕŖŗŘřŚśŜŝŞş', 'őőœœŕŕŗŗřřśśŝŝşş');
  AssertStringOperationUTF8LowerCase('Latin 0160 UTF8LowerCase', '', 'ŠšŢţŤťŦŧŨũŪūŬŭŮů', 'ššţţťťŧŧũũūūŭŭůů');
  AssertStringOperationUTF8LowerCase('Latin 0170 UTF8LowerCase', '', 'ŰűŲųŴŵŶŷŸŹźŻżŽžſ', 'űűųųŵŵŷŷÿźźżżžžſ');
  AssertStringOperationUTF8LowerCase('Latin 0180 UTF8LowerCase', '', 'ƀ Ɓ Ƃƃ Ƅƅ Ɔ Ƈƈ Ɖ Ɗ Ƌƌ ƍ Ǝ Ə', 'ƀ ɓ ƃƃ ƅƅ ɔ ƈƈ ɖ ɗ ƌƌ ƍ ǝ ə');
  AssertStringOperationUTF8LowerCase('Latin 0190 UTF8LowerCase', '', 'ƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟ', 'ɛƒƒɠɣƕɩɨƙƙƚƛɯɲƞɵ');
  AssertStringOperationUTF8LowerCase('Latin 01A0 UTF8LowerCase', '', 'ƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯ', 'ơơƣƣƥƥʀƨƨʃƪƫƭƭʈư');
  AssertStringOperationUTF8LowerCase('Latin 01B0 UTF8LowerCase', '', 'ưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿ', 'ưʊʋƴƴƶƶʒƹƹƺƻƽƽƾƿ');
  AssertStringOperationUTF8LowerCase('Latin 01C0 UTF8LowerCase', '', 'ǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏ', 'ǀǁǂǃǆǆǆǉǉǉǌǌǌǎǎǐ');
  AssertStringOperationUTF8LowerCase('Latin 01D0 UTF8LowerCase', '', 'ǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ', 'ǐǒǒǔǔǖǖǘǘǚǚǜǜǝǟǟ');
  AssertStringOperationUTF8LowerCase('Latin 01E0 UTF8LowerCase', '', 'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯ', 'ǡǡǣǣǥǥǧǧǩǩǫǫǭǭǯǯ');
  AssertStringOperationUTF8LowerCase('Latin 01F0 UTF8LowerCase', '', 'ǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿ', 'ǰǳǳǳǵǵƕƿǹǹǻǻǽǽǿǿ');
  AssertStringOperationUTF8LowerCase('Latin 0200 UTF8LowerCase', '', 'ȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏ', 'ȁȁȃȃȅȅȇȇȉȉȋȋȍȍȏȏ');
  AssertStringOperationUTF8LowerCase('Latin 0210 UTF8LowerCase', '', 'ȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟ', 'ȑȑȓȓȕȕȗȗșșțțȝȝȟȟ');
  AssertStringOperationUTF8LowerCase('Latin 0220 UTF8LowerCase', '', 'ȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯ', 'ƞȡȣȣȥȥȧȧȩȩȫȫȭȭȯȯ');
  AssertStringOperationUTF8LowerCase('Latin 0230 UTF8LowerCase', '', 'ȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿ', 'ȱȱȳȳȴȵȶȷȸȹⱥȼȼƚⱦȿ');
  AssertStringOperationUTF8LowerCase('Latin 0240 UTF8LowerCase', '', 'ɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏ', 'ɀɂɂƀʉʌɇɇɉɉɋɋɍɍɏɏ');
  AssertStringOperationUTF8LowerCase('Latin 0250 UTF8LowerCase', '', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ');
  AssertStringOperationUTF8LowerCase('Latin 0260 UTF8LowerCase', '', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ');
  AssertStringOperationUTF8LowerCase('Latin 0270 UTF8LowerCase', '', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ');
  AssertStringOperationUTF8LowerCase('Latin 0280 UTF8LowerCase', '', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ');
  AssertStringOperationUTF8LowerCase('Latin 0290 UTF8LowerCase', '', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ');
  AssertStringOperationUTF8LowerCase('Latin 02A0 UTF8LowerCase', '', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ');
  AssertStringOperationUTF8LowerCase('Unicode 0380 UTF8LowerCase', '', '΄΅Ά·ΈΉΊΌΎΏ', '΄΅ά·έήίόύώ');
  AssertStringOperationUTF8LowerCase('Unicode 0390 UTF8LowerCase', '', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ', 'ΐαβγδεζηθικλμνξο');
  AssertStringOperationUTF8LowerCase('Unicode 03A0 UTF8LowerCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'πρστυφχψωϊϋάέήί');
  AssertStringOperationUTF8LowerCase('Unicode 03B0 UTF8LowerCase', '', 'ΰαβγδεζηθικλμνξο', 'ΰαβγδεζηθικλμνξο');
  AssertStringOperationUTF8LowerCase('Unicode 03C0 UTF8LowerCase', '', 'πρςστυφχψωϊϋόύώϏ', 'πρςστυφχψωϊϋόύώϗ');
  AssertStringOperationUTF8LowerCase('Unicode 03D0 UTF8LowerCase', '', 'ϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟ', 'ϐϑϒϓϔϕϖϗϙϙϛϛϝϝϟϟ');
  AssertStringOperationUTF8LowerCase('Unicode 03E0 UTF8LowerCase', '', 'ϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯ', 'ϡϡϣϣϥϥϧϧϩϩϫϫϭϭϯϯ');
  AssertStringOperationUTF8LowerCase('Unicode 03F0 UTF8LowerCase', '', 'ϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿ', 'ϰϱϲϳθϵ϶ϸϸϲϻϻϼͻͼͽ');
  AssertStringOperationUTF8LowerCase('Unicode 0400 UTF8LowerCase', '', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ', 'ѐёђѓєѕіїјљњћќѝўџ');
  AssertStringOperationUTF8LowerCase('Unicode 0410 UTF8LowerCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'абвгдежзийклмноп');
  AssertStringOperationUTF8LowerCase('Unicode 0420 UTF8LowerCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'рстуфхцчшщъыьэюя');
  AssertStringOperationUTF8LowerCase('Unicode 0430 UTF8LowerCase', '', 'абвгдежзийклмноп', 'абвгдежзийклмноп');
  AssertStringOperationUTF8LowerCase('Unicode 0440 UTF8LowerCase', '', 'рстуфхцчшщъыьэюя', 'рстуфхцчшщъыьэюя');
  AssertStringOperationUTF8LowerCase('Unicode 0450 UTF8LowerCase', '', 'ѐёђѓєѕіїјљњћќѝўџ', 'ѐёђѓєѕіїјљњћќѝўџ');
  AssertStringOperationUTF8LowerCase('Unicode 0460 UTF8LowerCase', '', 'ѠѡѢѣѤѥѦѧѨѩѪѫѬѭѮѯ', 'ѡѡѣѣѥѥѧѧѩѩѫѫѭѭѯѯ');
  AssertStringOperationUTF8LowerCase('Unicode 0470 UTF8LowerCase', '', 'ѰѱѲѳѴѵѶѷѸѹѺѻѼѽѾѿ', 'ѱѱѳѳѵѵѷѷѹѹѻѻѽѽѿѿ');
  AssertStringOperationUTF8LowerCase('Unicode 0480 UTF8LowerCase', '', 'Ҁҁ҂ ҃ ҄ ҅ ҆ ҇ ҈ ҉ҊҋҌҍҎҏ', 'ҁҁ҂ ҃ ҄ ҅ ҆ ҇ ҈ ҉ҋҋҍҍҏҏ');
  AssertStringOperationUTF8LowerCase('Unicode 0490 UTF8LowerCase', '', 'ҐґҒғҔҕҖҗҘҙҚқҜҝҞҟ', 'ґґғғҕҕҗҗҙҙққҝҝҟҟ');
  AssertStringOperationUTF8LowerCase('Unicode 04A0 UTF8LowerCase', '', 'ҠҡҢңҤҥҦҧҨҩҪҫҬҭҮү', 'ҡҡңңҥҥҧҧҩҩҫҫҭҭүү');
  AssertStringOperationUTF8LowerCase('Unicode 04B0 UTF8LowerCase', '', 'ҰұҲҳҴҵҶҷҸҹҺһҼҽҾҿ', 'ұұҳҳҵҵҷҷҹҹһһҽҽҿҿ');
  AssertStringOperationUTF8LowerCase('Unicode 04C0 UTF8LowerCase', '', 'ӀӁӂӃӄӅӆӇӈӉӊӋӌӍӎӏ', 'ӏӂӂӄӄӆӆӈӈӊӊӌӌӎӎӏ');
  AssertStringOperationUTF8LowerCase('Unicode 04D0 UTF8LowerCase', '', 'ӐӑӒӓӔӕӖӗӘәӚӛӜӝӞӟ', 'ӑӑӓӓӕӕӗӗәәӛӛӝӝӟӟ');
  AssertStringOperationUTF8LowerCase('Unicode 04E0 UTF8LowerCase', '', 'ӠӡӢӣӤӥӦӧӨөӪӫӬӭӮӯ', 'ӡӡӣӣӥӥӧӧөөӫӫӭӭӯӯ');
  AssertStringOperationUTF8LowerCase('Unicode 04F0 UTF8LowerCase', '', 'ӰӱӲӳӴӵӶӷӸӹӺӻӼӽӾӿ', 'ӱӱӳӳӵӵӷӷӹӹӻӻӽӽӿӿ');
  AssertStringOperationUTF8LowerCase('Unicode 0500 UTF8LowerCase', '', 'ԀԁԂԃԄԅԆԇԈԉԊԋԌԍԎԏ', 'ԁԁԃԃԅԅԇԇԉԉԋԋԍԍԏԏ');
  AssertStringOperationUTF8LowerCase('Unicode 0510 UTF8LowerCase', '', 'ԐԑԒԓԔԕԖԗԘԙԚԛԜԝԞԟ', 'ԑԑԓԓԕԕԗԗԙԙԛԛԝԝԟԟ');
  AssertStringOperationUTF8LowerCase('Unicode 0520 UTF8LowerCase', '', 'ԠԡԢԣԤԥԦԧ', 'ԡԡԣԣԥԥԧԧ');
  // Armenian Unicode Table
  AssertStringOperationUTF8LowerCase('Unicode 0530 UTF8LowerCase', '', 'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿ', 'աբգդեզէըթժիլխծկ');
  AssertStringOperationUTF8LowerCase('Unicode 0540 UTF8LowerCase', '', 'ՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏ', 'հձղճմյնշոչպջռսվտ');
  AssertStringOperationUTF8LowerCase('Unicode 0550 UTF8LowerCase', '', 'ՐՑՒՓՔՕՖ', 'րցւփքօֆ');
  AssertStringOperationUTF8LowerCase('Unicode 0560 UTF8LowerCase', '', 'աբգդեզէըթժիլխծկ', 'աբգդեզէըթժիլխծկ');
  AssertStringOperationUTF8LowerCase('Unicode 0570 UTF8LowerCase', '', 'հձղճմյնշոչպջռսվտ', 'հձղճմյնշոչպջռսվտ');
  AssertStringOperationUTF8LowerCase('Unicode 0580 UTF8LowerCase', '', 'րցւփքօֆ', 'րցւփքօֆ');
  // Higher Unicode Table
  AssertStringOperationUTF8LowerCase('Unicode 1E00 UTF8LowerCase', '', 'ḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏ', 'ḁḁḃḃḅḅḇḇḉḉḋḋḍḍḏḏ');
  AssertStringOperationUTF8LowerCase('Unicode 1F00 UTF8LowerCase', '', 'ἀἁἂἃἄἅἆἇἈἉἊἋἌἍἎἏ', 'ἀἁἂἃἄἅἆἇἀἁἂἃἄἅἆἇ');
  AssertStringOperationUTF8LowerCase('Unicode 1F10 UTF8LowerCase', '', 'ἐἑἒἓἔἕἘἙἚἛἜἝ', 'ἐἑἒἓἔἕἐἑἒἓἔἕ');
  AssertStringOperationUTF8LowerCase('Unicode 1F20 UTF8LowerCase', '', 'ἠἡἢἣἤἥἦἧἨἩἪἫἬἭἮἯ', 'ἠἡἢἣἤἥἦἧἠἡἢἣἤἥἦἧ');
  AssertStringOperationUTF8LowerCase('Unicode 1F30 UTF8LowerCase', '', 'ἰἱἲἳἴἵἶἷἸἹἺἻἼἽἾἿ', 'ἰἱἲἳἴἵἶἷἰἱἲἳἴἵἶἷ');
  AssertStringOperationUTF8LowerCase('Unicode 1F40 UTF8LowerCase', '', 'ὀὁὂὃὄὅὈὉὊὋὌὍ', 'ὀὁὂὃὄὅὀὁὂὃὄὅ');
  AssertStringOperationUTF8LowerCase('Unicode 1F50 UTF8LowerCase', '', 'ὐὑὒὓὔὕὖὗὙὛὝὟ', 'ὐὑὒὓὔὕὖὗὑὓὕὗ');
  AssertStringOperationUTF8LowerCase('Unicode 1F60 UTF8LowerCase', '', 'ὠὡὢὣὤὥὦὧὨὩὪὫὬὭὮὯ', 'ὠὡὢὣὤὥὦὧὠὡὢὣὤὥὦὧ');
  AssertStringOperationUTF8LowerCase('Unicode 1F70 UTF8LowerCase', '', 'ὰάὲέὴήὶίὸόὺύὼώ', 'ὰάὲέὴήὶίὸόὺύὼώ');
  AssertStringOperationUTF8LowerCase('Unicode 1F80 UTF8LowerCase', '', 'ᾀᾁᾂᾃᾄᾅᾆᾇᾈᾉᾊᾋᾌᾍᾎᾏ', 'ᾀᾁᾂᾃᾄᾅᾆᾇᾀᾁᾂᾃᾄᾅᾆᾇ');
  AssertStringOperationUTF8LowerCase('Unicode 1F90 UTF8LowerCase', '', 'ᾐᾑᾒᾓᾔᾕᾖᾗᾘᾙᾚᾛᾜᾝᾞᾟ', 'ᾐᾑᾒᾓᾔᾕᾖᾗᾐᾑᾒᾓᾔᾕᾖᾗ');
  AssertStringOperationUTF8LowerCase('Unicode 1FA0 UTF8LowerCase', '', 'ᾠᾡᾢᾣᾤᾥᾦᾧᾨᾩᾪᾫᾬᾭᾮᾯ', 'ᾠᾡᾢᾣᾤᾥᾦᾧᾠᾡᾢᾣᾤᾥᾦᾧ');
  // Turkish
  AssertStringOperationUTF8LowerCase('Turkish UTF8LowerCase 1', 'tr', 'abcçdefgğhııijklmnoöprsştuüvyz', 'abcçdefgğhııijklmnoöprsştuüvyz');
  AssertStringOperationUTF8LowerCase('Turkish UTF8LowerCase 2', 'tr', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'abcçdefgğhııijklmnoöprsştuüvyz');
  AssertStringOperationUTF8LowerCase('Turkish UTF8LowerCase 3', 'tr', 'AhıIxXa', 'ahııxxa');
  // Cyrillic
  AssertStringOperationUTF8LowerCase('Russian UTF8LowerCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertStringOperationUTF8LowerCase('Russian UTF8LowerCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertStringOperationUTF8LowerCase('Cyrillic UTF8UpperCase 1', '', 'Ѡѡ Ѣѣ Ѥѥ Ѧѧ Ѩѩ Ѫѫ Ѭѭ Ѯѯ Ѱѱ Ѳѳ Ѵѵ Ѷѷ Ѹѹ Ѻѻ Ѽѽ Ѿѿ Ҁҁ', 'ѡѡ ѣѣ ѥѥ ѧѧ ѩѩ ѫѫ ѭѭ ѯѯ ѱѱ ѳѳ ѵѵ ѷѷ ѹѹ ѻѻ ѽѽ ѿѿ ҁҁ');
  AssertStringOperationUTF8LowerCase('Cyrillic UTF8UpperCase 2', '', 'Ҋҋ Ҍҍ Ҏҏ Ґґ Ғғ Ҕҕ Җҗ Ҙҙ Ққ Ҝҝ Ҟҟ Ҡҡ Ңң Ҥҥ Ҧҧ Ҩҩ Ҫҫ Ҭҭ Үү Ұұ Ҳҳ Ҵҵ Ҷҷ Ҹҹ Һһ Ҽҽ Ҿҿ', 'ҋҋ ҍҍ ҏҏ ґґ ғғ ҕҕ җҗ ҙҙ ққ ҝҝ ҟҟ ҡҡ ңң ҥҥ ҧҧ ҩҩ ҫҫ ҭҭ үү ұұ ҳҳ ҵҵ ҷҷ ҹҹ һһ ҽҽ ҿҿ');
  // What shouldnt change
  AssertStringOperationUTF8LowerCase('Chinese UTF8LowerCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');
  // Georgian
  AssertStringOperationUTF8LowerCase('Georgian UTF8LowerCase 1', '', 'Ⴀⴀ Ⴁⴁ Ⴂⴂ Ⴃⴃ Ⴄⴄ Ⴅⴅ Ⴆⴆ Ⴇⴇ Ⴈⴈ Ⴉⴉ Ⴊⴊ Ⴋⴋ Ⴌⴌ Ⴍⴍ Ⴎⴎ Ⴏⴏ Ⴐⴐ Ⴑⴑ', 'ⴀⴀ ⴁⴁ ⴂⴂ ⴃⴃ ⴄⴄ ⴅⴅ ⴆⴆ ⴇⴇ ⴈⴈ ⴉⴉ ⴊⴊ ⴋⴋ ⴌⴌ ⴍⴍ ⴎⴎ ⴏⴏ ⴐⴐ ⴑⴑ');
  AssertStringOperationUTF8LowerCase('Georgian UTF8LowerCase 2', '', 'Ⴒⴒ Ⴓⴓ Ⴔⴔ Ⴕⴕ Ⴖⴖ Ⴗⴗ Ⴘⴘ Ⴙⴙ Ⴚⴚ Ⴛⴛ Ⴜⴜ Ⴝⴝ Ⴞⴞ Ⴟⴟ Ⴠⴠ Ⴡⴡ Ⴢⴢ Ⴣⴣ Ⴤⴤ Ⴥⴥ', 'ⴒⴒ ⴓⴓ ⴔⴔ ⴕⴕ ⴖⴖ ⴗⴗ ⴘⴘ ⴙⴙ ⴚⴚ ⴛⴛ ⴜⴜ ⴝⴝ ⴞⴞ ⴟⴟ ⴠⴠ ⴡⴡ ⴢⴢ ⴣⴣ ⴤⴤ ⴥⴥ');

  // repeat all tests with leading turkish i, to force offset
  // ASCII
  AssertStringOperationUTF8LowerCase('Offset ASCII UTF8LowerCase', 'tr', 'IABCDEFGHIJKLMNOPQRSTUWVXYZ', 'ıabcdefghıjklmnopqrstuwvxyz');
  // Latin
  AssertStringOperationUTF8LowerCase('Offset Portuguese UTF8LowerCase 1', 'tr', 'IÇ/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'ıç/ç ã/ã õ/õ á/á é/é í/í ó/ó ú/ú ü/ü à/à â/â ê/ê î/î ô/ô û/û');
  AssertStringOperationUTF8LowerCase('Offset French UTF8LowerCase 1', 'tr', 'IÀ/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'ıà/à â/â æ ç/ç é/é è/è ê/ê ë/ë î/î ï/ï ô/ô œ ù/ù û/û ü/ü ÿ/ÿ');
  AssertStringOperationUTF8LowerCase('Offset Polish UTF8LowerCase 1', 'tr', 'Iaąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'ıaąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('Offset Polish UTF8LowerCase 2', 'tr', 'IAĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'ıaąbcćdeęfghıjklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('Offset German UTF8LowerCase 1', 'tr', 'IÄ/ä,Ö/ö,Ü/ü,ß', 'ıä/ä,ö/ö,ü/ü,ß');
  // Turkish
  AssertStringOperationUTF8LowerCase('Offset Turkish UTF8LowerCase 1', 'tr', 'Iabcçdefgğhııijklmnoöprsştuüvyz', 'ıabcçdefgğhııijklmnoöprsştuüvyz');
  AssertStringOperationUTF8LowerCase('Offset Turkish UTF8LowerCase 2', 'tr', 'IABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'ıabcçdefgğhııijklmnoöprsştuüvyz');
  AssertStringOperationUTF8LowerCase('Offset Turkish UTF8LowerCase 1', 'tr', 'IAhıIxXa', 'ıahııxxa');
  // Cyrillic
  AssertStringOperationUTF8LowerCase('Offset Russian UTF8LowerCase 1', 'tr', 'IАБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'ıабвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertStringOperationUTF8LowerCase('Offset Russian UTF8LowerCase 2', 'tr', 'Iабвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'ıабвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertStringOperationUTF8LowerCase('Offset Cyrillic UTF8UpperCase 1', 'tr', 'IѠѡ Ѣѣ Ѥѥ Ѧѧ Ѩѩ Ѫѫ Ѭѭ Ѯѯ Ѱѱ Ѳѳ Ѵѵ Ѷѷ Ѹѹ Ѻѻ Ѽѽ Ѿѿ Ҁҁ', 'ıѡѡ ѣѣ ѥѥ ѧѧ ѩѩ ѫѫ ѭѭ ѯѯ ѱѱ ѳѳ ѵѵ ѷѷ ѹѹ ѻѻ ѽѽ ѿѿ ҁҁ');
  AssertStringOperationUTF8LowerCase('Offset Cyrillic UTF8UpperCase 2', 'tr', 'IҊҋ Ҍҍ Ҏҏ Ґґ Ғғ Ҕҕ Җҗ Ҙҙ Ққ Ҝҝ Ҟҟ Ҡҡ Ңң Ҥҥ Ҧҧ Ҩҩ Ҫҫ Ҭҭ Үү Ұұ Ҳҳ Ҵҵ Ҷҷ Ҹҹ Һһ Ҽҽ Ҿҿ', 'ıҋҋ ҍҍ ҏҏ ґґ ғғ ҕҕ җҗ ҙҙ ққ ҝҝ ҟҟ ҡҡ ңң ҥҥ ҧҧ ҩҩ ҫҫ ҭҭ үү ұұ ҳҳ ҵҵ ҷҷ ҹҹ һһ ҽҽ ҿҿ');
  // What shouldnt change
  AssertStringOperationUTF8LowerCase('Offset Chinese UTF8LowerCase 1', 'tr', 'I名字叫嘉英，嘉陵江的嘉，英國的英', 'ı名字叫嘉英，嘉陵江的嘉，英國的英');
  // Georgian
  AssertStringOperationUTF8LowerCase('Offset Georgian UTF8LowerCase 1', 'tr', 'IႠⴀ Ⴁⴁ Ⴂⴂ Ⴃⴃ Ⴄⴄ Ⴅⴅ Ⴆⴆ Ⴇⴇ Ⴈⴈ Ⴉⴉ Ⴊⴊ Ⴋⴋ Ⴌⴌ Ⴍⴍ Ⴎⴎ Ⴏⴏ Ⴐⴐ Ⴑⴑ', 'ıⴀⴀ ⴁⴁ ⴂⴂ ⴃⴃ ⴄⴄ ⴅⴅ ⴆⴆ ⴇⴇ ⴈⴈ ⴉⴉ ⴊⴊ ⴋⴋ ⴌⴌ ⴍⴍ ⴎⴎ ⴏⴏ ⴐⴐ ⴑⴑ');
  AssertStringOperationUTF8LowerCase('Offset Georgian UTF8LowerCase 2', 'tr', 'IႲⴒ Ⴓⴓ Ⴔⴔ Ⴕⴕ Ⴖⴖ Ⴗⴗ Ⴘⴘ Ⴙⴙ Ⴚⴚ Ⴛⴛ Ⴜⴜ Ⴝⴝ Ⴞⴞ Ⴟⴟ Ⴠⴠ Ⴡⴡ Ⴢⴢ Ⴣⴣ Ⴤⴤ Ⴥⴥ', 'ıⴒⴒ ⴓⴓ ⴔⴔ ⴕⴕ ⴖⴖ ⴗⴗ ⴘⴘ ⴙⴙ ⴚⴚ ⴛⴛ ⴜⴜ ⴝⴝ ⴞⴞ ⴟⴟ ⴠⴠ ⴡⴡ ⴢⴢ ⴣⴣ ⴤⴤ ⴥⴥ');

  // Performance test
  Write('Mattias LowerCase- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCaseViaTables('abcdefghijklmnopqrstuwvxyz');
      if j = 1 then Str := UTF8LowerCaseViaTables('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
      if j = 2 then Str := UTF8LowerCaseViaTables('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
      if j = 3 then Str := UTF8LowerCaseViaTables('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
      if j = 4 then Str := UTF8LowerCaseViaTables('АБВЕЁЖЗКЛМНОПРДЙГ');
      if j = 5 then Str := UTF8LowerCaseViaTables('名字叫嘉英，嘉陵江的嘉，英國的英');
      if j = 6 then Str := UTF8LowerCaseViaTables('AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz');
      if j = 7 then Str := UTF8LowerCaseViaTables('AAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm');
      if j = 8 then Str := UTF8LowerCaseViaTables('abcDefgHijkLmnoPqrsTuwvXyz');
      if j = 9 then Str := UTF8LowerCaseViaTables('ABCdEFGhIJKlMNOpQRStUWVxYZ');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  writeln;
  Write('       LowerCase-- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCase('abcdefghijklmnopqrstuwvxyz');
      if j = 1 then Str := UTF8LowerCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
      if j = 2 then Str := UTF8LowerCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
      if j = 3 then Str := UTF8LowerCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
      if j = 4 then Str := UTF8LowerCase('АБВЕЁЖЗКЛМНОПРДЙГ');
      if j = 5 then Str := UTF8LowerCase('名字叫嘉英，嘉陵江的嘉，英國的英');
      if j = 6 then Str := UTF8LowerCase('AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz');
      if j = 7 then Str := UTF8LowerCase('AAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm');
      if j = 8 then Str := UTF8LowerCase('abcDefgHijkLmnoPqrsTuwvXyz');
      if j = 9 then Str := UTF8LowerCase('ABCdEFGhIJKlMNOpQRStUWVxYZ');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  writeln;
  Write('   Turk LowerCase-- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCase('Iabcdefghijklmnopqrstuwvxyz', 'tr');
      if j = 1 then Str := UTF8LowerCase('IABCDEFGHIJKLMNOPQRSTUWVXYZ', 'tr');
      if j = 2 then Str := UTF8LowerCase('Iaąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'tr');
      if j = 3 then Str := UTF8LowerCase('IAĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'tr');
      if j = 4 then Str := UTF8LowerCase('IАБВЕЁЖЗКЛМНОПРДЙГ', 'tr');
      if j = 5 then Str := UTF8LowerCase('I名字叫嘉英，嘉陵江的嘉，英國的英', 'tr');
      if j = 6 then Str := UTF8LowerCase('IAaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz', 'tr');
      if j = 7 then Str := UTF8LowerCase('IAAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm', 'tr');
      if j = 8 then Str := UTF8LowerCase('IabcDefgHijkLmnoPqrsTuwvXyz', 'tr');
      if j = 9 then Str := UTF8LowerCase('IABCdEFGhIJKlMNOpQRStUWVxYZ', 'tr');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  writeln;
end;

begin
  WriteLn('======= UpperCase =======');
  TestUTF8UpperCase();
  WriteLn('======= LowerCase =======');
  TestUTF8LowerCase();
  WriteLn('Please press enter to continue');
  readln;
end.

