;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; ** Standard HTML stylesheets

;;;; We include these as variables so that whene generating an html
;;;; doc set we needn't know the location of qbook's source code.

(defvar *print.css*
  "body {
  background-color: #FFFFFF;
  padding: 0px; margin: 0px;
}

.qbook { 
  width: 600px; 
  background-color: #FFFFFF; 
  padding: 0em;
  margin: 0px; 
}

h1, h2, h3, h4, h5, h6 {
  font-family: verdana; 
}

h1 { 
  text-align: center; 
  padding: 0px;
  margin: 0px;
}

h2 { 
  text-align: center;
  border-top: 1px solid #000000; 
  border-bottom: 1px solid #000000; 
}

h3, h4, h5, h6 { 
  border-bottom: 1px solid #000000; 
  padding-left: 1em; 
}

h3 { border-top: 1px solid #000000; }

p { padding-left: 1em; }

pre.code {
  border: solid 1px #FFFFFF;
  padding: 2px;
  overflow: visible; 
}

pre .first-line-more-link { display: none; }

pre.code * .paren  { color: #666666; } 

pre.code a:active  { color: #000000; }
pre.code a:link    { color: #000000; }
pre.code a:visited { color: #000000; }

pre.code .first-line { font-weight: bold; }

pre.code .body, pre.code * .body { display: inline; }

div.contents { 
  font-family: verdana; 
  border-bottom: 1em solid #333333;
  margin-left: -0.5em;
}

div.contents a:active  { color: #000000; }
div.contents a:link    { color: #000000; }
div.contents a:visited { color: #000000; }

div.contents div.contents-heading-1 { padding-left: 0.5em; font-weight: bold; }
div.contents div.contents-heading-1 a:active  { color: #333333; }
div.contents div.contents-heading-1 a:link    { color: #333333; }
div.contents div.contents-heading-1 a:visited { color: #333333; }

div.contents div.contents-heading-2 { padding-left: 1.0em; }
div.contents div.contents-heading-2 a:active  { color: #333333; }
div.contents div.contents-heading-2 a:link    { color: #333333; }
div.contents div.contents-heading-2 a:visited { color: #333333; }

div.contents div.contents-heading-3 { padding-left: 1.5em; }
div.contents div.contents-heading-3 a:active  { color: #333333; }
div.contents div.contents-heading-3 a:link    { color: #333333; }
div.contents div.contents-heading-3 a:visited { color: #333333; }

div.contents div.contents-heading-4 { padding-left: 2em; }
div.contents div.contents-heading-4 a:active  { color: #333333; }
div.contents div.contents-heading-4 a:link    { color: #333333; }
div.contents div.contents-heading-4 a:visited { color: #333333; }

div.contents div.contents-heading-5 { padding-left: 2.5em; }
div.contents div.contents-heading-5 a:active  { color: #333333; }
div.contents div.contents-heading-5 a:link    { color: #333333; }
div.contents div.contents-heading-5 a:visited { color: #333333; }

.footer { float: bottom-right; color: #000000; font-family: arial; font-size: small; }
.footer a:active { color: #000000; }
.footer a:link { color: #000000; }
.footer a:visited { color: #000000; }
"
  "The alternative (destined for hard copy) HTML stylesheet.")

(defvar *style.css*
  "body {
  background-color: #FFFFFF;
  padding: 0px; 
  margin: 0px;
  font-family: verdana;
}

.qbook { 
  margin: auto;
  background-color: #FFFFFF; 
  width: 40em;
}

h1, h2, h3, h4, h5, h6 {
  color: #990000;
}

h1 { 
  text-align: center; 
  padding: 0px;
  margin: 0px;
}

h2 { 
  text-align: center;
  border-bottom: 5px solid #CC0000; 
  margin-top: 2em;
}

h3, h4, h5, h6 {
  padding-left: 1em;
  margin-top: 2em;
}

h3 { 
  border-bottom: 2px solid #CC0000; 
}

h4, h5, h6 { 
  border-bottom: 1px solid #CC0000; 
}

pre.code {
  background-color: #eeeeee;
  border: solid 1px #d0d0d0;
  overflow: auto;
}

pre.code * .paren { color: #666666; } 

pre.code .first-line { font-weight: bold; }

pre.code .body, pre.code * .body { display: none; }

div.contents { 
  font-family: verdana; 
}

a:active  { color: #0000AA; }
a:link    { color: #0000AA; }
a:visited { color: #0000AA; }

div.contents div.contents-heading-1 { padding-left: 0.5em; font-weight: bold; }
div.contents div.contents-heading-1 a:active  { color: #333333; }
div.contents div.contents-heading-1 a:link    { color: #333333; }
div.contents div.contents-heading-1 a:visited { color: #333333; }

div.contents div.contents-heading-2 { padding-left: 1.0em; }
div.contents div.contents-heading-2 a:active  { color: #333333; }
div.contents div.contents-heading-2 a:link    { color: #333333; }
div.contents div.contents-heading-2 a:visited { color: #333333; }

div.contents div.contents-heading-3 { padding-left: 1.5em; }
div.contents div.contents-heading-3 a:active  { color: #333333; }
div.contents div.contents-heading-3 a:link    { color: #333333; }
div.contents div.contents-heading-3 a:visited { color: #333333; }

div.contents div.contents-heading-4 { padding-left: 2em; }
div.contents div.contents-heading-4 a:active  { color: #333333; }
div.contents div.contents-heading-4 a:link    { color: #333333; }
div.contents div.contents-heading-4 a:visited { color: #333333; }

div.contents div.contents-heading-5 { padding-left: 2.5em; }
div.contents div.contents-heading-5 a:active  { color: #333333; }
div.contents div.contents-heading-5 a:link    { color: #333333; }
div.contents div.contents-heading-5 a:visited { color: #333333; }

.footer { color: #000000; font-family: arial; font-size: small; }
.footer a:active { color: #000000; }
.footer a:link { color: #000000; }
.footer a:visited { color: #000000; }

div.contents div.computational-element-link { padding-left: 1em; }

.nav-links { font-size: x-small; float: right; margin-top: -2em; }

table.permuted-index-table {
  border-spacing: 0px;
}

table.permuted-index-table tr td {
  font-size: x-small;
  padding: 0px;
}
"
  "The default stylesheet for qbook generated html documentation.")