{include resources/header.md}
{set-property title "metabang-bind - Sticking it the to metal..."}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html


</div>
<div class="system-description">

### What it is

Bind combines _let_, _destructuring-bind_ and _multiple-value-bind_ and a whole lot more into a single form. Simple bindings are as in _let*_. Destructuring is done if the first item in a binding is a list. Multiple value binding is done if the first item in a binding is a list and the first item in the list is 'values'.

An example is probably the best way to describe its syntax:
    
    (bind ((a 2)
           ((b &rest args &key (c 2) &allow-other-keys) '(:a :c 5 :d 10 :e 54))
           ((:values d e) (truncate 4.5)))
      (list a b c d e args))
    ==> (2 :A 5 4 0.5 (:C 5 :D 10 :E 54))

Bind is especially handy when you have more than one layer of multiple-value-bind or destructuring-bind. Since bind is a single form, you don't end up too far off to the right in editor land.

Bind works by parsing the bindings and rewriting them as them as necessary. Bind handles declarations correctly -- putting each at the appropriate level.

Bind is released under the MIT license.

{anchor mailing-lists}

### Mailing Lists

metabang-bind isn't a separate project at this point but if you have questions, comments or concerns, just drop [Gary King][8] a line.

   [8]: mailto:gwking@metabang.com

{anchor downloads}

### Where is it

A [Darcs][9] repository is available. The commands are listed below:
    
   [9]: http://www.darcs.net/

    darcs get http://common-lisp.net/project/metabang-bind/

metabang-bind is also [ASDF installable][10]. Its CLiki home is right [where][11] you'd expect.

   [10]: http://www.cliki.net/asdf-install
   [11]: http://www.cliki.net/bind

There's also a handy [gzipped tar file][12].

   [12]: http://common-lisp.net/project/cl-containers/metabang-bind/metabang-bind_latest.tar.gz

{anchor news}

### What is happening

13 Nov 2005 - Initial webpage n' stuff.

</div>
</div>

{include resources/footer.md}


