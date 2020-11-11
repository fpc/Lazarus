// Add defines here. This file should be included in all LazUtils units headers


{$undef UTF8_RTL}  // FPC >= 2.7.1 with codepages and default string = CP_UTF8
{$undef ACP_RTL}   // FPC >= 2.7.1 with codepages and default string = CP_ACP


{$ifndef DisableUTF8RTL}
  {$define UTF8_RTL}
{$else DisableUTF8RTL}
  {$define ACP_RTL}
{$endif DisableUTF8RTL}

