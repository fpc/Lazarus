unit ipro_tests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  LE = LineEnding;
  
//------------------------------------------------------------------------------
//   <BR> tag
//------------------------------------------------------------------------------
const
  BRinBODY_title = 
    '<BR> between two words in BODY';
  BRinBODY_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  abc<br>def' + LE +
    '</body>' + LE +
    '</html>';
  BRinBODY_descr =
    'The two words should be in separate lines, having no additional empty line between.';

  TwoBRinBODY_title = 
    'Two <BR> tags between two words in BODY';
  TwoBRinBODY_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  abc<br><br>def' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinBODY_descr =
    'The two words should be in separate lines with additional empty line between.';

  BRinP_title = 
    '<BR> between two words in P nodes';
  BRinP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc<br>def</p>' + LE +
    '</body>' + LE +
    '</html>';
  BRinP_descr =
    'The two words should be in separate lines, having no additional empty line between.';
  
  TwoBRinP_title = 
    'Two <BR> tags between two words in P nodes';
  TwoBRinP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc<br><br>def</p>' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinP_descr =
    'The two words should be in separate lines with an additional empty line between.';

  BRinTableCell_title = 
    '<BR> between two words in a table cell';
  BRinTableCell_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr><td>abc<br>def</td><td>ghi</td></tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  BRinTableCell_descr =
    'The two words in the left cell should be in separate lines, having no additional empty line between.';
  
  TwoBRinTableCell_title = 
    'Two <BR> tags between two words in a table cell';
  TwoBRinTableCell_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr><td>abc<br><br>def</td><td>ghi</td></tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinTableCell_descr =
    'The two words in the left cell should be in separate lines with an additional empty line between.';

  BRbetweenTwoTables_title = 
    '<BR> between two tables';
  BRbetweenTwoTables_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE + 
    '      <td>abc</td><td>def</td><td>ghi</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '  <br>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE + 
    '      <td>ABC</td><td>DEF</td><td>GHI</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  BRbetweenTwoTables_descr =
    'There should be an empty line between the two tables.';
    
  BRbetweenTwoP_title = 
    '<BR> between two <p> tags';
  BRbetweenTwoP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc</p>' + LE +
    '  <br>' + LE +
    '  <p>ABC</p>' + LE +
    '</body>' + LE +
    '</html>';
  BRbetweenTwoP_descr =
    'There should be an empty line between the two lines.';

  
// -----------------------------------------------------------------------------
//   Background color
//------------------------------------------------------------------------------
const
  TextWithBackgroundInBODY_title =
    'Text with background in BODY';
  TextWithBackgroundInBODY_html =
    '<html>' + LE +
    '<body>' + LE +
    '  <font style="background-color: yellow;" color="red">Testing backcolor</font>' + LE +
    '</body>' + LE +
    '</html>';
  TextWithBackgroundInBODY_descr =
    'This test should show red text on yellow background.';

  
  TextWithBackgroundInBODY_CSS_title =
    'Text with background in BODY (with CSS)';
  TextWithBackgroundInBODY_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    .class1 {' + LE +
    '      color: red;' + LE +
    '      background-color: yellow;' + LE +
    '    }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <p class="class1">Testing backcolor</p>' + LE +
    '</body>' + LE +
    '</html>';
  TextWithBackgroundInBODY_CSS_descr =
    'This test should show red text on yellow background.';

  TextInColoredTableCell_title =
    'Text in colored table cell';
  TextInColoredTableCell_html = 
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr bgcolor="yellow">' + LE +
    '      <td>abc</td>' + LE +
    '      <td>def</td>' + LE +
    '      <td bgcolor="red">Test</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  TextInColoredTableCell_descr =
    'The left and center cells should have yellow, the right cell uniform red background.';

//------------------------------------------------------------------------------
//   Text Alignment
//------------------------------------------------------------------------------
const
  AlignInCell_title =
    'Left-/right-aligned and centered text in a table cell';
  AlignInCell_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCell_html =
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left">a</td>' + LE +
    '      <td align="center">b</td>' + LE +
    '      <td align="right">c</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCellBold_title =
    'Left-/right-aligned and centered BOLD text in a table cell';
  AlignInCellBold_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCellBold_html =
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left"><b>a</b></td>' + LE +
    '      <td align="center"><b>b</b></td>' + LE +
    '      <td align="right"><b>c</b></td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCell_CSS_title =
    'Left-/right-aligned and centered text in a table cell, using body style';
  AlignInCell_CSS_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCell_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    body { font-size:9pt; font-family:Arial, Helvetica, sans-serif }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0" style="font-size:9pt; font-family:Arial, Helvetica, sans-serif;">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left">a</td>' + LE +
    '      <td align="center">b</td>' + LE +
    '      <td align="right">c</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCellBold_CSS_title =
    'Left-/right-aligned and centered BOLD text in a table cell, using body style';
  AlignInCellBold_CSS_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCellBold_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    body { font-size:9pt; font-family:Arial, Helvetica, sans-serif }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0" style="font-size:9pt; font-family:Arial, Helvetica, sans-serif;">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left"><b>a</b></td>' + LE +
    '      <td align="center"><b>b</b></td>' + LE +
    '      <td align="right"><b>c</b></td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';


implementation

end.

