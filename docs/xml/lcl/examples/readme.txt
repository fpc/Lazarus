This directory is intended for storing examples of Pascal code included in the documentation.

You can include example code in the documentation as follows:

1. Place an example of the code inside this directory as a unit with name UnitName.FunctionName.pas or similar.
   Place the link in this example for the documentation <element> tag as follows:

   <element name="MyDangerousFunction">
    <example file="examples/UnitName.MyDangerousFunction.pas"/>
   </element>

   The file attribute of the <example> tag is used to specify the filename containing the example. The filename does not require an extension: if it is missing, .pp is assumed.

   If you use the fpdoc editor then use the example tab for that.
   https://www.freepascal.org/docs-html/current/fpdoc/fpdocsu70.html

2. Place the example code into the <descr><descr/> section inside <code></code> tags.
    Note that any text surrounding the code tags should be placed inside <p></p> tags.
    For example:

    <element name="MyDangerousFunction">
    <descr>
    <p>
        // Any description.
    </p>
    <code>
        // Place example code here.
    </code>
    </descr>
    </element>

    https://www.freepascal.org/docs-html/current/fpdoc/fpdocsu63.html

