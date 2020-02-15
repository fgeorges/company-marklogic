xquery version "3.1";

(: Generate the tables for company-marklogic.el.
 :
 : There are 2 versions of the tables, one for XQuery and the other one for JavaScript.
 : It depends on the list of functions available at http://docs.marklogic.com/.  Adapt
 : $dir depending on where you downloaded it.
 :
 : Executing the module will produce text.  Replace the last line to choose between
 : either the XQuery or JavaScript tables.  The result is to be pasted in the file
 : company-marklogic-xqy.el (resp. company-marklogic-sjs.el.)  It is the entire content
 : of the corresponding file.
 :)

declare namespace out = "http://www.w3.org/2010/xslt-xquery-serialization"; 
declare namespace ad  = "http://marklogic.com/xdmp/apidoc";

declare option out:method "text"; 

declare variable $dir  := 'file:/c:/home/doc/MarkLogic_9_pubs/pubs/raw/apidoc/';
declare variable $docs := fn:collection($dir)/ad:module;

declare variable $modules := (
    for $doc in $docs
    return
        <module filename="{ fn:substring-after(fn:document-uri($doc/..), $dir) }"> {
            $doc/@*,
            $doc/ad:function ! <function> {
                @*,
                <name type="xqy">{ xs:string(@name) }</name>,
                <name type="sjs">{ local:javascriptify(@name) }</name>
            }
            </function>
        }
        </module>,
    (: list retrieved from "3 Built-in Datatypes and Their Definitions", in the table of
       content titles themselves, at https://www.w3.org/TR/xmlschema11-2/#contents :)
    <module>
        <function lib="xs"><name type="xqy">ENTITIES</name><name type="sjs">ENTITIES</name></function>
        <function lib="xs"><name type="xqy">ENTITY</name><name type="sjs">ENTITY</name></function>
        <function lib="xs"><name type="xqy">ID</name><name type="sjs">ID</name></function>
        <function lib="xs"><name type="xqy">IDREF</name><name type="sjs">IDREF</name></function>
        <function lib="xs"><name type="xqy">IDREFS</name><name type="sjs">IDREFS</name></function>
        <function lib="xs"><name type="xqy">NCName</name><name type="sjs">NCName</name></function>
        <function lib="xs"><name type="xqy">NMTOKEN</name><name type="sjs">NMTOKEN</name></function>
        <function lib="xs"><name type="xqy">NMTOKENS</name><name type="sjs">NMTOKENS</name></function>
        <function lib="xs"><name type="xqy">NOTATION</name><name type="sjs">NOTATION</name></function>
        <function lib="xs"><name type="xqy">Name</name><name type="sjs">Name</name></function>
        <function lib="xs"><name type="xqy">QName</name><name type="sjs">QName</name></function>
        <function lib="xs"><name type="xqy">anyAtomicType</name><name type="sjs">anyAtomicType</name></function>
        <function lib="xs"><name type="xqy">anySimpleType</name><name type="sjs">anySimpleType</name></function>
        <function lib="xs"><name type="xqy">anyURI</name><name type="sjs">anyURI</name></function>
        <function lib="xs"><name type="xqy">base64Binary</name><name type="sjs">base64Binary</name></function>
        <function lib="xs"><name type="xqy">boolean</name><name type="sjs">boolean</name></function>
        <function lib="xs"><name type="xqy">byte</name><name type="sjs">byte</name></function>
        <function lib="xs"><name type="xqy">date</name><name type="sjs">date</name></function>
        <function lib="xs"><name type="xqy">dateTime</name><name type="sjs">dateTime</name></function>
        <function lib="xs"><name type="xqy">dateTimeStamp</name><name type="sjs">dateTimeStamp</name></function>
        <function lib="xs"><name type="xqy">dayTimeDuration</name><name type="sjs">dayTimeDuration</name></function>
        <function lib="xs"><name type="xqy">decimal</name><name type="sjs">decimal</name></function>
        <function lib="xs"><name type="xqy">double</name><name type="sjs">double</name></function>
        <function lib="xs"><name type="xqy">duration</name><name type="sjs">duration</name></function>
        <function lib="xs"><name type="xqy">float</name><name type="sjs">float</name></function>
        <function lib="xs"><name type="xqy">gDay</name><name type="sjs">gDay</name></function>
        <function lib="xs"><name type="xqy">gMonth</name><name type="sjs">gMonth</name></function>
        <function lib="xs"><name type="xqy">gMonthDay</name><name type="sjs">gMonthDay</name></function>
        <function lib="xs"><name type="xqy">gYear</name><name type="sjs">gYear</name></function>
        <function lib="xs"><name type="xqy">gYearMonth</name><name type="sjs">gYearMonth</name></function>
        <function lib="xs"><name type="xqy">hexBinary</name><name type="sjs">hexBinary</name></function>
        <function lib="xs"><name type="xqy">int</name><name type="sjs">int</name></function>
        <function lib="xs"><name type="xqy">integer</name><name type="sjs">integer</name></function>
        <function lib="xs"><name type="xqy">language</name><name type="sjs">language</name></function>
        <function lib="xs"><name type="xqy">long</name><name type="sjs">long</name></function>
        <function lib="xs"><name type="xqy">negativeInteger</name><name type="sjs">negativeInteger</name></function>
        <function lib="xs"><name type="xqy">nonNegativeInteger</name><name type="sjs">nonNegativeInteger</name></function>
        <function lib="xs"><name type="xqy">nonPositiveInteger</name><name type="sjs">nonPositiveInteger</name></function>
        <function lib="xs"><name type="xqy">normalizedString</name><name type="sjs">normalizedString</name></function>
        <function lib="xs"><name type="xqy">positiveInteger</name><name type="sjs">positiveInteger</name></function>
        <function lib="xs"><name type="xqy">short</name><name type="sjs">short</name></function>
        <function lib="xs"><name type="xqy">string</name><name type="sjs">string</name></function>
        <function lib="xs"><name type="xqy">time</name><name type="sjs">time</name></function>
        <function lib="xs"><name type="xqy">token</name><name type="sjs">token</name></function>
        <function lib="xs"><name type="xqy">unsignedByte</name><name type="sjs">unsignedByte</name></function>
        <function lib="xs"><name type="xqy">unsignedInt</name><name type="sjs">unsignedInt</name></function>
        <function lib="xs"><name type="xqy">unsignedLong</name><name type="sjs">unsignedLong</name></function>
        <function lib="xs"><name type="xqy">unsignedShort</name><name type="sjs">unsignedShort</name></function>
        <function lib="xs"><name type="xqy">yearMonthDuration</name><name type="sjs">yearMonthDuration</name></function>
    </module>
);

declare variable $prefixes :=
    (: 'require' and 'declareUpdate' are handled differently, and REST API are not relevant :)
    let $ignore := ('Global-Object', 'manage', 'rest-client')
    for $p in fn:distinct-values($modules/function/@lib)[fn:not(. = $ignore)]
    order by $p
    return
        $p;

declare function local:capitalize($s as xs:string) as xs:string
{
    fn:upper-case(fn:substring($s, 1, 1)) || fn:substring($s, 2)
};

declare function local:javascriptify($name as xs:string) as xs:string
{
    let $parts := fn:tokenize($name, '-')
    return
        fn:string-join((
            fn:head($parts),
            fn:tail($parts) ! local:capitalize(.)))
};

declare function local:alist-entry($p as xs:string, $type as xs:string) as xs:string
{
    '(' || $p || ' . ,(addp "' || $p || '" company-marklogic-' || $type || '-functions-' || $p || '))'
};

declare function local:format-tables($type as xs:string, $sep as xs:string, $label as xs:string) as text()+
{
    text { ';;; company-marklogic-' || $type || '.el --- ' || $label || ' tables for Company MarkLogic backend.&#10;&#10;' },
    text { ';; See company-marklogic.el for copyright info.&#10;&#10;' },
    for $p in $prefixes
    let $f := for $i in $modules/function[@lib eq $p]/name[@type eq $type] ! xs:string(.) order by $i return $i
    return (
        text { '(defconst company-marklogic-' || $type || '-functions-' || $p || '&#10;' }, 
        text { '  ''("' || fn:head($f) || '"' },
        fn:tail($f) ! text { '&#10;    "' || . || '"' },
        text { '))&#10;&#10;' }
    ),
    text { '(defconst company-marklogic-' || $type || '-functions&#10;' },
    text { '  (cl-labels ((addp (prefix names)&#10;' },
    text { '                (mapcar (lambda (n) (concat prefix "' || $sep || '" n)) names)))&#10;' },
    text { '  `(' || local:alist-entry(fn:head($prefixes), $type) },
    fn:tail($prefixes) ! text { '&#10;    ' || local:alist-entry(., $type) },
    text { ')))&#10;&#10;' },
    text { '(provide ''company-marklogic-' || $type || ')&#10;' }
};

(:
local:format-tables('xqy', ':', 'XQuery')
local:format-tables('sjs', '.', 'JavaScript')
:)
local:format-tables('xqy', ':', 'XQuery')
