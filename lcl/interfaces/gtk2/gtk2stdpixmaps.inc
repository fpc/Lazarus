{%MainUnit gtk2themes.pas}

const
  XPM_MDIRESTOREBUTTON: array[0..12] of PChar =
  (
    '10 10 2 1',
    ' 	c None',
    '.	c #000000',
    '          ',
    '   .......',
    '   .......',
    '   .     .',
    ' ....... .',
    ' ....... .',
    ' .     ...',
    ' .     .  ',
    ' .     .  ',
    ' .......  '
  );

  XPM_MDICLOSEBUTTON: array[0..12] of PChar =
  (
    '10 10 2 1',
    ' 	c None',
    '.	c #000000',
    '          ',
    ' ..    .. ',
    ' ...  ... ',
    '  ......  ',
    '   ....   ',
    '   ....   ',
    '  ......  ',
    ' ...  ... ',
    ' ..    .. ',
    '          '
  );

  XPM_MDIMINBUTTON: array[0..12] of PChar =
  (
    '10 10 2 1',
    ' 	c None',
    '.	c #000000',
    '          ',
    '          ',
    '          ',
    '          ',
    '          ',
    '          ',
    '          ',
    ' ......   ',
    ' ......   ',
    '          '
  );

var
  PixmapArray: array[1..3] of PPgchar =
  (
{1 - MDIMINBUTTON}
  @XPM_MDIMINBUTTON,
{2 - MDIRESTOREBUTTON}
  @XPM_MDIRESTOREBUTTON,
{3 - MDICLOSEBUTTON}
  @XPM_MDICLOSEBUTTON
  );
