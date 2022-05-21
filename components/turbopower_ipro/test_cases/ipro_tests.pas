unit ipro_tests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  LE = LineEnding;
  
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

  TextWithBackgroundInBODY_title =
    'Text with background in BODY';
  TextWithBackgroundInBODY_html =
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
  TextWithBackgroundInBODY_descr =
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


implementation

end.

