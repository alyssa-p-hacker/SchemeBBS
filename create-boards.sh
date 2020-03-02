#!/bin/sh
DATADIR="./data"
[ $# -eq 0 ] && { echo "Usage: $0 boardname..."; exit 1; }
for board in $@; do
  if [ -f "$DATADIR/html/$board/index" ]; then
    echo "board $board already exists"
  else
      mkdir -p "$DATADIR/sexp/$board"
      mkdir -p "$DATADIR/html/$board"
      echo "()" > "$DATADIR/sexp/$board/index"
      echo '<!DOCTYPE HTML PUBLIC "ISO/IEC 15445:2000//DTD HyperText Markup Language//EN">
<HTML>
<HEAD>
<TITLE>/'"$board"'/ - SchemeBBS</TITLE>
<META content="text/html; charset=UTF-8" http-equiv="Content-Type">
<META name="viewport" content="width=device-width, initial-scale=1.0">
<LINK rel="icon" href="/static/favicon.ico" type="image/png">
<LINK href="/static/styles/default.css" rel="stylesheet" type="text/css"></HEAD>
<BODY>
<H1>'"$board"'</H1>
<P class="nav">frontpage - <A href="/'"$board"'/list">thread list</A> - <A href="#newthread">new thread</A> - <A href="/'"$board"'/preferences">preferences</A> - <A href="http://textboard.org">?</A></P>
<HR>
<H2 id="newthread">New Thread</H2>
<FORM action="/'"$board"'/post" method="post">
<P class="newthread"><LABEL for="titulus">Headline</LABEL><BR>
<INPUT type="text" name="titulus" id="titulus" size="78" maxlength="78" value=""><BR>
<LABEL for="epistula">Message</LABEL><BR>
<TEXTAREA name="epistula" id="epistula" rows="12" cols="77"></TEXTAREA><INPUT type="hidden" name="ornamentum" value="3b3738ae1c9295d272cec84ecf7af5c8"><BR>
<INPUT type="submit" value="POST"></P>
<FIELDSET class="comment"><LEGEND>do not edit these</LEGEND>
<P><INPUT type="text" name="name" class="name" size="11"><BR>
<TEXTAREA name="message" class="message" rows="1" cols="11"></TEXTAREA></P></FIELDSET></FORM>
<HR>
<P class="footer">bbs.scm + <A href="https://www.gnu.org/software/mit-scheme/">MIT Scheme</A> + <A href="https://mitpress.mit.edu/sites/default/files/sicp/index.html">SICP</A> + Satori Mode</P></BODY></HTML>' > "$DATADIR/html/$board/index"
  fi
done
