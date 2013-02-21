# Contributing 
## to Source code
to be added...

## to Documentation
The documentation lives in the CouchDB source tree. We’ll start by 
forking and closing the CouchDB GitHub mirror. That will allow us to 
send the contribution to CouchDB with a pull request.

If you don’t have a GitHub account yet, it is a good time to get one, 
they are free. If you don’t want to use GitHub, there are alternate 
ways to contributing back, that we’ll cover next time.

Go to https://github.com/apache/couchdb and click the “fork” button in 
the top right. This will create a fork of CouchDB in your GitHub 
account. Mine is janl, so my fork lives at 
https://github.com/janl/couchdb. In the header, it tells me me my 
“GitHub Clone URL”. We need to copy that and start a terminal:

I’m opening the whole CouchDB source tree in my favourite editor. It 
gives me the usual directory listing:

The documentation sources live in src/doc, you can safely ignore all 
the other files and directories.

First we should determine where we want to document this inside the 
documentation. We can look through http://docs.couchdb.org/en/latest/ 
for inspiration. The JSON Structure Reference looks like a fine place 
to write this up.

The current state includes mostly tables describing the JSON structure 
(after all, that’s the title of this chapter), but some prose about the 
number representation can’t hurt. For future reference, since the topic 
in the thread includes views and different encoding in views (as 
opposed to the storage engine), we should remember to make a note in 
the views documentation as well, but we’ll leave this for later.

Let’s try and find the source file that builds the file 
http://docs.couchdb.org/en/latest/json-structure.html – we are in luck, 
under share/docs/src/ we find the file json-structure.rst. That looks 
promising. .rst stands for ReStructured Text (see 
http://thomas-cokelaer.info/tutorials/sphinx/rest_syntax.html for a 
markup reference), which is an ascii format for writing documents, 
documentation in this case. Let’s have a look and open it.

We see ascii tables with some additional formatting, all looking like 
the final HTML. So far so easy. For now, let’s just add to the bottom 
of this. We can worry about organising this better later.

We start by adding a new headline:
```
Number Handling
===============
```
Now we paste in the rest of the main email of the thread. It is mostly 
text, but it includes some code listings. Let’s mark them up. We’ll 
turn:
```
ejson:encode(ejson:decode(<<"1.1">>)).
<<"1.1000000000000000888">>
```
Into:
```
.. code-block:: erlang

  ejson:encode(ejson:decode(<<"1.1">>)).
  <<"1.1000000000000000888">>
```
And we follow along with the other code samples. We turn:
```
Spidermonkey

$ js -h 2>&1 | head -n 1
JavaScript-C 1.8.5 2011-03-31
$ js
js> JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
"1.0123456789012346"
js> var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
js> JSON.stringify(JSON.parse(f))
"1.0123456789012346"
```
into:
```
Spidermonkey::

    $ js -h 2>&1 | head -n 1
    JavaScript-C 1.8.5 2011-03-31
    $ js
    js> JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    "1.0123456789012346"
    js> var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    js> JSON.stringify(JSON.parse(f))
    "1.0123456789012346"
```
And then follow all the other ones.

I cleaned up the text a little but to make it sound more like a 
documentation entry as opposed to a post on a mailing list.

The next step would be to validate that we got all the markup right. 
I’ll leave this for later. For now we’ll contribute our change back to 
CouchDB.

First, we commit our changes: 
```
$ > git commit -am 'document number encoding' 
[master a84b2cf] 
document number encoding 
1 file changed, 199 insertions(+)
```
Then we push the commit to our CouchDB fork: 
```
$ git push origin master
```
Next, we go back to our GitHub page https://github.com/janl/couchdb 
and click the “Pull Request button”. Fill in the description with 
something useful and hit the “Send Pull Request” button.

And we’re done!
