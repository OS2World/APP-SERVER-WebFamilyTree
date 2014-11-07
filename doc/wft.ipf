:userdoc.
:title.WFT documentation
:docprof toc=123.

.***********************************
.*   INTRODUCTION
.***********************************

:h1.Introduction

:hp2.WEB FAMILY TREE:ehp2.

:p.
WFT stands for "web family tree". It is intended for use by people who
(a) want to display their family tree in a web browser, and (b) run
their own web server, or are willing to install a web server.

:p.
This, by the way, includes people who don't have a permanent internet
connection, and also those who want to keep their data private
and not display it on the global internet. It is possible to install
a web server that responds only to the so-called "loopback connection",
i.e. that serves data only to client software running on the same
machine. See the :link reftype=hd refid=privacy.privacy:elink.
section for other privacy-related issues.

:p.
The easiest way to see whether WFT suits your needs is to take a look at my own
family tree. Go to
:xmp.
     http&colon.&slash.&slash.eepjm.newcastle.edu.au/os2/wft.html
:exmp.
and follow the 'Try it out' link.

:p.
This software is :hp2.freeware:ehp2.. You don't have to pay me for it, and you
can even obtain a copy of the source code by going to
http&colon.&slash.&slash.eepjm.newcastle.edu.au. The only restriction I am
placing on its distribution and use is that you do not engage in
plagiarism. If you re-use my code in a commercial application, you
must first ask my permission, and negotiate a suitable licencing agreement.
If you re-use my code in a freeware
application you do not need my permission, but you must include a
reference to the original source, and you must tell me that you
are using it.

:p.This documentation is for version 1.2.

:p.
:hp2.Disclaimer of Warranty:ehp2.

:sl compact.
:li.
:hp1.
This Product is provided "as-is", without warranty of any
kind, either expressed or implied, including, but not limited to,
the implied warranties of merchantability and fitness for a
particular purpose. The entire risk as to the quality and
performance of the Product is with you. Should the Product prove
defective, the full cost of repair, servicing, or correction lies
with you.
:ehp1.
:esl.

:p.
The author of WFT is Peter Moylan, peter@ee.newcastle.edu.au.

:p.
The latest version of WFT is normally kept at ftp&colon.&slash.&slash.eepjm.newcastle.edu.au/software.
Information about other software on this site may be found at
http&colon.&slash.&slash.eepjm.newcastle.edu.au/os2.

:p.
:hp2.Getting information about new versions:ehp2.

:p.
You can, if you wish, join a mailing list for announcements about
new releases of my software. The mailing list called
software-announce@eepjm.newcastle.edu.au is used purely for such
announcements. (It has very little traffic - merely a message each
time I release a new version of one of my programs.) To join
this list, send an
e-mail to majormajor@eepjm.newcastle.edu.au. The subject line is not
important and may be anything. In the body of the message, put the
lines
:xmp.

       subscribe software-announce
       end
:exmp.

:p.To have yourself removed from the list, send a similar e-mail but
using the command "unsubscribe" instead of "subscribe".

.***********************************
.*   PREREQUISITES
.***********************************

:h1 id=prerequisites.Prerequisites

:hp2.Prerequisites:ehp2.

:p.
In order to run this software, you must have
:ul.
:li.an OS/2 or eCS operating system.
:li.a web server running on your machine.
:eul.

:p.You probably satisfy the first requirement (otherwise, why
download this package?), but you might not yet have a web
server running on your machine.

:p.For the web server, I personally recommend the one that you
will find at http&colon.&slash.&slash.dink.org, but that is just
one of many possibilities. (I have no connection with dink.org,
other than being a satisfied user.) Some other popular web
servers are mentioned in the section on
:link reftype=hd refid=webserver.Installing a web server:elink..
WFT is almost certainly
compatible with any of the web servers available for the
OS/2 and eCS platforms.

.***********************************
.*   INSTALLATION
.***********************************

:h1.Installation
:hp2.Installation:ehp2.
:p.
See also :link reftype=hd refid=upgrading.Upgrading:elink.
and :link reftype=hd refid=deinstall.De-installation:elink..

:p.Before you use this program, you need to have a web server
installed. If you do not already have a web server on your computer,
go to the section about
:link reftype=hd refid=webserver.installing a web server:elink.
before continuing to install WFT.

:p.You should have received this package in the form of a zip file.
The first step is to unzip the file into a directory of your choice.
(Presumably you have already done this.) You will find that this
directory contains a file called INSTALL.CMD, plus a few other
files.

:p.WFT requires the following setup:
:ul.
:li.The program itself goes into a directory of your own choice.
:li.The GEDCOM files - that is, the files that define the data for
your family tree - go into a subdirectory of that directory,
called 'data'.
:li.To interface with the web server, a small Rexx CGI script must
be put in the web server's cgi-bin directory.
:eul.

:p.The script Install.CMD puts
all the files into the right places. It lets you choose to install to
the current directory, or a different directory, depending on your
own preference. It also creates a second script called
Remove.CMD, that you can use to uninstall the program if you
decide you don't want it.

:p.If you prefer to do a manual installation, just read Install.CMD to
see what it does.

.***********************************
.*   UPGRADING
.***********************************

:h2 id=upgrading.Upgrading
:hp2.Upgrading:ehp2.

:p.If you have an earlier version of WFT installed, you can safely
install this version in the same directory. Your existing data
subdirectory will not be modified. That is, you can either
:ul.
:li.unzip this version into the directory that contains the older
version; or
:li.run INSTALL.CMD, and specify the older version's directory as
the directory into which the new version should be installed.
:eul.

.***********************************
.*   INSTALLING A WEB SERVER
.***********************************

:h2 id=webserver.Installing a web server
:hp2.Installing a web server:ehp2.
:p.
This software will not work unless you are running a web server.
If you are already running a web server on your computer, you
can ignore this section. Otherwise, you must obtain and install
a web server before installing WFT.

:p.
The web server can be set up either as a "real"
web server that is accessible to everyone, or as a private server
that talks only to your own computer via the loopback interface.
The loopback interface is the one with IP address [127.0.0.1],
and its name is usually "localhost". If the loopback interface
is not already working for you (try the command "ping 127.0.0.1"),
you should open the TCP/IP configuration notebook and enable it.

:p.There is a good choice of web servers in the
/pub/os2/apps/internet/www/server directory at hobbes.nmsu.edu.
Most of them are free. The one I recommend is Web/2, available from
http&colon.&slash.&slash.dink.org/, because it is particularly easy to install, and also
because it is faster and more compact than the other web servers I have tried.
However, the choice of web server appears to be a religious issue.
Some other people would recommend Apache or GoServe, or
perhaps a few others. Any of these is suitable for
supporting WFT, provided only that the server supports the use
of CGI scripts. Every web server I have ever seen has CGI support,
so you can choose whichever one you like best.

.***********************************
.*    HEADERS AND FOOTERS
.***********************************

:h2 id=headerfooter.Headers and footers
:hp2.Headers and footers:ehp2.

:p.
Optionally, your WFT directory may contain files called
"header" and "footer". If they exist, then the contents of
the "header" file are written at the beginning of every web
page generated by WFT, and the contents of the "footer" file
are written at the end of every page.

:p.This package includes
sample header and footer files, as examples, but I have renamed them
'sampleheader' and 'samplefooter' so that they do not accidentally
overwrite header and footer files that you have already customized
for your own use.

:p.
These files may contain standard HTML markup. They may also
contain the special codes&colon.
:dl tsize=10 break=fit.
:dt.     %%
:dd.the literal character '%'
:dt.     %d
:dd.today's date
:dt.     %ifilename
:dd.the contents of file 'filename'. If this parameter is not
terminated by a space character or end-of-line, the filename
should be enclosed in single ('') or double ("") quotation marks.
:dt.     %v
:dd.the version number of WFT
:edl.

:p.Suppose that the file you are displaying is called 'name.ged'.
If files called 'data\name.header' and/or 'data\name.footer' exist, these
will replace the 'header' and 'footer' files respectively. This
allows you to have different headers and footers for different
databases. If you also want to include the global headers and
footers, you can use the %i'header' and/or %i'footer' macros.

:note.Header and footer files that belong to specific databases
(the name.header case) should go into the 'data' directory where
the GED files belong. Global header and footer files, if you use
them, belong in the same directory as WFT.EXE. In addition, filenames
in the %i command are interpreted relative to the directory in
which WFT.EXE lives, unless of course you use absolute file names.

.***********************************
.*   LANGUAGE SUPPORT
.***********************************

:h2 id=language.Language support
:hp2.Language support:ehp2.
:p.
Most of what WFT displays is language-independent, but there are
some labels that have to be in a specific language. These labels
are defined in files wft.*.lng, where the central part of the
file name is a language code. (These files have to reside in the
same directory as WFT.EXE.) For example, the English-language
labels are in the file wft.en.lng.

:p.
It is easy to add support for another language. Just take one of
the existing wft.*.lng files, copy it over with a new name that
reflects the language, and translate the quoted strings in that
new file.

:p.The language that the user sees is not controlled from your
end. It is, instead, specified in the language preferences in
the user's web browser. (In Mozilla, for example, use the menu
item Edit/Preferences, go to the Navigator section, and you will
find a page called Languages.) WFT searches through the user's
preferences until it finds a language code xy such that there
is a WFT language file called wft.xy.lng. If it is impossible
to find any supported language in the user's list, the file
wft.en.lng is used.

:p.This multilanguage support has, so far, only been tested with
the Web/2 web server. I don't yet know what other servers will
support it.

.***********************************
.*   DEINSTALLATION
.***********************************

:h1 id=deinstall.De-installation
:hp2.De-installation:ehp2.
:p.
If you installed WFT using the script INSTALL.CMD, it will have
created a new script called REMOVE.CMD in the WFT directory.
If you run that script, WFT will be removed from your computer.

:p.
If you did a manual installation, rather than running INSTALL.CMD,
you can deinstall WFT as follows&colon.
:ul.
:li.Delete the file WFT.CMD from your web server's cgi-bin directory.
:li.In the directory into which you installed WFT, the subdirectory
'data' probably contains valuable data that you want to keep.
Copy these files to a suitable backup directory.
:li.Delete the directory into which you installed WFT.
:eul.

:p.WFT does not modify CONFIG.SYS or the system INI files,
so you do not have to modify those when de-installing WFT.

.***********************************
.*   RUNNING THE PROGRAM
.***********************************

:h1 id=runningWFT.Running the program
:hp2.Running the program:ehp2.

:p.
WFT is not intended to be 'runnable' in the usual sense of the word.
If you try to execute WFT.EXE from a command line, the results will be
cryptic and a little disappointing.

:p.
Instead, WFT runs as a CGI program that can be invoked by the web
server. CGI, or Common Gateway Interface, is a mechanism for
letting web servers run programs on behalf of the client. The
usual convention is that anything in the cgi-bin directory on the
server is considered to be an executable program rather than
an HTML file.

:p.
The client invokes such a program with an http&colon.&slash.&slash. reference
that refers to the cgi-bin directory. A typical client URL would be
:xmp.

   http&colon.&slash.&slash.eepjm.newcastle.edu.au/cgi-bin/wft.cmd?D=moylan;P=I004
:exmp.

:p.
This runs the script wft.cmd that is found in the cgi-bin directory.
If you look at that script, you will see that it is a Rexx script
that in turn runs the program WFT.EXE. We could have put that
WFT.EXE into the cgi-bin directory, but I don't believe in
cluttering up the cgi-bin directory with lots of rubbish. After a
while you can lose track of what is in there, which means that
you can lose track of what clients can be doing to your machine.

:p.
The part after the '?' in the URL specifies a parameter string
that gets passed to WFT.EXE. WFT allows several parameters,
separated by the ';' character. The possible parameter values are
as follows.

:dl break=all.
:dt.     D=name
:dd.This says that the data will be taken from a data file
called name.GED. The 'D' parameter is compulsory, to
let WFT know which database to read. (You are
allowed to have several different GED files in your data directory.)
:dt.     P=personID
:dd.This says which person you are looking up. The personID
is a reference to a line in the GED file of the form
:xmp.
               0 @personID@ INDI
:exmp.
which is the GEDCOM way of starting a record for this person.
:p.The 'P' parameter is optional. If it is missing, the program
will return data for the first person found in the file.
:dt.     V=code
:dd.This optional parameter specifies a different view of the
data. The possible values for the code are&colon.
:dl.
:dt.                  D
:dd.show the descendants of this person
:dt.                  D+
:dd.show the descendants of this person, with all details
:dt.                  A
:dd.show the ancestors of this person
:dt.                  A+
:dd.show the ancestors of this person, with all details
:dt.                  E
:dd.show everyone in the database
:dt.                  E+
:dd.show everyone in the database, with all details
:edl.

:p.NOTE: the 'P' parameter is compulsory in the first four of
these cases, but not in the 'E' or 'E+' cases.
:edl.

.***********************************
.*   INDEX FILES
.***********************************

:h2 id=indexfiles.Index files
:hp2.Index files:ehp2.

:p.
For each data file NAME.GED in your data directory, the program
creates a corresponding NAME.IDX. This is an index file for more
efficient lookup of the data. To save you the trouble of having
to do a separate 'indexing' operation, the program automatically
generates this file whenever it sees that it is missing. Most of
the time you don't have to worry about whether it is there or not.

:p.If you edit the GED file, the corresponding index file will contain
incorrect information. The program will rebuild the index file
any time it detects an inconsistency; but, for efficiency reasons,
it does not do a complete consistency check, so it will sometimes
get obsolete information from the index file. The result will
be that the program displays wrong information, e.g. it might
look as if someone is his own father, or it might appear that
there is only one person in the database. If you see this happening
you should manually delete the IDX file, which will force the
program to create a new version.

:p.An even safer approach would be to delete the IDX file every
time you edit the data.

.***********************************
.*   WHY DID I DO IT THIS WAY?
.***********************************

:h1 id=WhyDidI.Why did I do it this way?
:hp2.Why did I do it this way?:ehp2.

:p.The decision to use a GEDCOM file to store the genealogical
data is non-controversial. GEDCOM is the format of choice of
almost all genealogical researchers. The fact that it appears to
have been designed by Windows users is a non-issue here; there
is no comparable non-Windows standard.

:p.My approach of rendering the GEDCOM date as a text-mode web
page is less evident. If you search the web, you will find two
major approaches to displaying genealogical data:

:ol.
:li.Most Windows users use software that translates GEDCOM data
into HTML files, so that the results are displayed as "normal"
web pages.
:li.The non-Windows developers tend to use Java applets as the
display mechanism.
:eol.

:p.My objection to the Java applets is simply that they are slow.
The results are good, but the time delays are unacceptable.

:p.My objection to the Windows approaches is that they are
'too public'. If a database is rendered into pure HTML, and made
available on the web, it then becomes available to search engines
like Google. In my experience, some people will refuse to have
their data included under these conditions. (And who can blame them,
given the current trend for some countries to attempt to destroy the civil
rights of their citizens?)

:p.
My compromise, therefore, is
:ul.
:li.to export only text-mode data, so that it will be rendered without
an unreasonable time delay.
:li.to export the data in such a way that it will not be found by
search engines such as Google. That is, private data will remain
uncatalogued by the search engines.

:eul.

.***********************************
.*   EDITING YOUR DATA
.***********************************

:h1 id=EditingData.Editing your data
:hp2.Editing your data:ehp2.

:p.To use this program, you need your family tree in GEDCOM format.
You can create this with a plain text editor, but it would be a
tedious job. A better approach would be to get a program
like GenJ, available from http&colon.&slash.&slash.www.meiers.net/nils/.
If you don't like that one, a web search on terms like 'GEDCOM' will
turn up various other editors.

:p.Strictly speaking, you don't need WFT once you have GenJ, because
GenJ has its own way, using a Java applet, of displaying the results in
a web browser. I wrote WFT because I personally find the GenJ
approach to be too elaborate; I prefer a 'plain text' output.
That, in any case, is a matter of personal taste. Try them both
and see what you prefer. WFT and GenJ are both freeware, so
it doesn't cost you anything to shop around.

:p.:hp2.What is GEDCOM format?:ehp2.

:p.GEDCOM is a data representation standard that is very popular
for storing genealogical data. It was created by The Church of
Jesus Christ of Latter-Day Saints, a church that has a strong
interest in collecting genealogical information and that has
large historical databases. You don't have to be a member of the
LDS church to use its standard. If you are the sort of person who
likes reading standards documents, you can find the standard
by doing a web search for "GEDCOM Standard". The home of the standard
is at http&colon.//www.familysearch.org, but I have found that it is
easier to find copies at other web sites.

:p.If you exchange genealogical files with other people, there is a
very high probability that they will already be in GEDCOM format.

:p.Note that the version of GEDCOM supported by WFT is version 5.5,
also known as "Traditional GEDCOM". There is a draft version 6 that
uses XML and is totally different in terms of document format. As
far as I can tell, the XML version is not so far well supported
by genealogy software.

:p.:hp2.Specifying the character encoding:ehp2.

:p.The GEDCOM standard allows a choice of three character encodings,
called ASCII, ANSEL, and UNICODE. Earlier versions of the standard
also allowed IBMPC. Unfortunately all of these have problems&colon.

:ul.

:li.ASCII is acceptable if you stick to plain English data, but
it can't handle things like accented characters.

:li.IBMPC is ambiguous. Does it mean IBM850, or IBM852, etc.?

:li.UNICODE is also ambiguous. When people say they are using
Unicode, it usually turns out they are using UTF-7 or UTF-8.

:li.ANSEL is the only unambiguous one, but because it is not
used for anything except GED files there is a risk that
you won't find an editor that understands it, and you'll be
stuck doing obscure manual tweaks. In addition, it is biased
towards a subset of Western European languages, so there is
a risk that it won't support all of the characters you are
interested in.
:eul.

:p.In the longer term, the growing adoption of Unicode will probably
sort out these problems. In the short term, WFT has a workaround
that should work for many people. The first record in a GED
file is a header record, and it should contain a level 1 header
line labelled CHAR. WFT will use that as the character set
specification. For example, if your data are recorded in Hebrew,
you could edit that line to read
:xmp.
   1 CHAR ISO-8859-8-1
:exmp.
(To be honest, I suspect that particular example will not
work, because I have no experience with rendering right-to-left
scripts. It is possible that the names will be rendered
backwards.)

:p.The character sets that you can safely use in the CHAR lines are
the ones supported by popular web browsers. To get an idea of what
should work, view a web page that uses your preferred script, and
then use the Mozilla View->Page Info menu option to see what
character encoding it is using.

:p.Note that some family tree editors, for example GenJ, will
"correct" your character set specification to one that you don't want.
Luckily, it's a simple matter to use a plain text editor, after
doing the main editing job, to repair the "correction".

:p.:hp2.Cross-references between Gedcom files:ehp2.

:p.You might find that your family tree data is being maintained
by several different people. (In my case, I'm dealing with
several separate branches of the family, researched by five
different people.) Of course,
it's not hard to develop software that will do a "merge" of
two or more databases, but that has two disadvantages:
:ul compact.
:li.it can create very large files; and
:li.it ignores the fact that the original files are probably
being updated from time to time.
:eul.

:p.The WFT solution to that is to permit cross-references between
files. The mechanism is explained on the
:link reftype=hd refid=crossref.following page:elink.. Although
this is not yet supported by the GEDCOM standard, it seems
likely that some future revision of the standard will support
a similar mechanism.

.***********************************
.*   CREATING A CROSS-REFERENCE
.***********************************

:h2 id=crossref.Creating a cross-reference
:hp2.Creating a cross-reference:ehp2.

:p.
The present GEDCOM standard does not, as far as I know, allow for
cross-references between GEDCOM files. To create such linkages,
I have found it necessary to add an extension to the standard.
I don't like doing this, and I intend to revert to the standard way
once a standard way exists, but for now it's a useful workaround,
and it might even turn out to be compatible with future revisions
of the standard.

:p.The GEDCOM standard allows for entity references of the form
:xmp.
         @id@
:exmp.
where 'id' is an identifier assigned by the person (or software)
creating the database. This sort of reference occurs in FAMC and
FAMS and CHIL records, and probably others I can't think of for
now. The extension I have allowed for is also to have references
of the form
:xmp.
         @database&colon.id@
:exmp.
such that this refers to the identity referred to as 'id' in the
file 'database.ged'. This seems to be compatible with the
standard, which specifies that the '&colon.' character may not be used
in any 'id', presumably in the expectation of an extension like
this one in some future revision of the standard.

:p.The point of doing this is that you might have two or more GEDCOM
files, and you don't want to merge them because they are
maintained by different people. You can create the cross-references
with some minor manual editing, and from then on WFT can jump
across files as needed.

:p.Warning: the versions of GenJ I have tested will, for some unknown
reason, delete the '@' delimiters from cross-references of this form.
You can fix this by running the :link reftype=hd refid=utilities.Fixgen:elink.
utility after editing a file with GenJ.

.***********************************
.*   MISCELLANEOUS UTILITIES
.***********************************

:h1 id=utilities.Miscellaneous utilities
:hp2.Miscellaneous utilities:ehp2.

:p.
The following programs are not needed for WFT to work. They
are simply extra utilities that you may use if you wish.

:p.:hp2.The GEDSORT utility:ehp2.

:p.The WFT zip file also contains a program called GEDSORT.EXE.
When you run this program you
specify one parameter, which is the name of a GED file. For
example,
:xmp.

               GedSort data\Moylan

:exmp.
to sort the Moylan.GED file. The purpose of this utility is to
sort the individuals in the GEDCOM file into alphabetical order.
This does not change anything important (because the GEDCOM
standard does not care about the ordering of the records), but
it can give a neater look to the 'Display everybody' option of WFT.
It can also make records easier to find if you have to edit
them manually.

:p.:hp2.The LINT utility:ehp2.

:p.This is a program that checks a GED file for some common errors,
such as having a loop in the tree. Again, you specify one
filename parameter when you run this program. It does not alter
the data file, it simply writes the results of its analysis
to standard output (usually the screen).

:p.:hp2.The SUBSET utility:ehp2.

:p.This takes two parameters: the name of an existing GED file, and
the label of an INDI or FAM record within that file. The result,
which is written to standard output, is a subset of the original
file. The result file contains the specified individual or family,
and all descendants of that individual or family, but ancestors
are eliminated, together with all nodes that are reachable in the
family tree only via those ancestors. An example of using it is
:xmp.
      subset data\wholefamily.ged I049 >data\partfamily.ged
:exmp.
In effect, what you get is a subtree of the original tree, but it
is not strictly speaking a tree, because it also includes things
like the ancestors of the spouses of the descendants of the original
specified node.

:p.:hp2.The TIDY utility:ehp2.

:p.This takes one parameter, which is the name of an existing GED
file. This program deletes some of the useless lines from the file.
For example, a BIRT subrecord that does not contain a date or
place is useless, and can be removed. The result is to make the
file a little smaller, without throwing away any meaningful
information.

:p.:hp2.The FIXGEN utility:ehp2.

:p.This program takes the name of one GEDCOM file as its only parameter.
It repairs three problems that can be caused by editing your GEDCOM
file with the GenealogyJ program&colon.
:ul.
:li.GenJ deletes your CHAR specification and replaces it with
CHAR IBMPC (which is of course ambiguous). The original CHAR line
is lost, so this program reads the first line from a file called
CHAR from the current directory to find the character set you really want.
:li.Surnames containing the '/' character have an extra space
character inserted. Fixgen removes the space.
:li.External links, i.e. those containing a '&colon.' character, have
had the surrounding '@' delimiters deleted. Fixgen reinserts the
missing '@' delimiters.
:eul.

.***********************************
.*   PRIVACY
.***********************************

:h1 id=privacy.Privacy
:hp2.Privacy:ehp2.

:p.Some people object to having their name listed on a web page.
This might or might not turn out to be an issue for you. In any
case, this page discusses several ways of restricting how visible
your family tree data will be.

:p.:hp2.Running a non-public web server:ehp2.

:p.It is possible to install a web server on a machine whose only
network connection is the so-called "loopback" connection back
to itself. This creates a one-machine network whose only machine
has the hostname "localhost" and IP address 127.0.0.1. You can
build up an entire web site at address http&colon.//localhost,
and it will be invisible to everyone except for web browsers on
the local machine. In this case, the web browser is, in effect,
acting as a purely local application program.

:p.A similar example is provided when you have a private network,
either completely isolated from the public internet or protected
from it via a firewall. As long as the firewall is blocking the
HTTP port (usually port 80, but you could use a nonstandard port
if you wished), nobody from outside the private
network can see your web pages.

:p.Actually, it's fairly unlikely that your web pages will be found,
even if your server has an address on the Internet, if you never
advertise its address. People generally don't try random addresses
just to see whether a web server is present. Neither do search
engines; a search engine finds new addresses by following links
from the pages it already knows about. Some "hacker" software does
search random addresses, but in that case it's looking for
security holes. To avoid that sort of attack, you just have to
ensure that you're not running a version of a server that has
known vulnerabilities.

:p.:hp2.Keeping your WFT pages out of search engines:ehp2.

:p.For most people, an acceptable level of privacy can be had by
ensuring that your WFT pages are never indexed by search engines.
That way humans can read the pages if they know your site
address, but people unknown to you are unlikely to find those
same pages.

:p.The way to control this is to have a file called ROBOTS.TXT
in the root directory of your HTTP tree. (That is, in the
directory that you have configured as your "HTML" directory
when setting up the web server.) Ethical search engines will
skip the pages that ROBOTS.TXT tells them to skip. As for the
non-ethical search engines, you guard against them by never
putting e-mail addresses on web pages, because e-mail addresses
are what they are searching for.

:p.The format of the ROBOTS.TXT entries can easily be found via
a web search. As an example, here is my ROBOTS.TXT file.

:xmp.
User-agent: msnbot/*
Disallow: /

User-agent: *
Disallow: /cgi-bin/
:exmp.

:p.The first of these two entries says that no version of msnbot
may index any page on my web site. This has nothing to do with
WFT; msnbot is the Microsoft search engine, and if we ban it from
as many web sites as possible then we will delay the day when
Microsoft attempts to kill off all competing search engines. The
second entry is the important one. It says that :hp3.no:ehp3. search
engine may follow the links that start with "/cgi-bin", and this
includes all links generated by WFT.

:p.:hp2.Finer privacy control:ehp2.

:p.In addition to the above, WFT follows the following rules:

:ul.
:li.Level 1 ADDR records are never displayed by WFT. These contain
addresses, probably as a reminder to you of how to contact that
person. You can use other software - even a plain text editor - to
look up those addresses yourself, but it's probably not a good
idea to reveal them to everyone else.
:li.If an INDI record contains a level 1 'RESN PRIVACY' record,
that individual can be displayed when encountered (as part of
a family, for example), but it will not be displayed as a
hypertext link, so you can't follow that link to find out more
about that person. This is an extreme measure, because it
effectively makes descendants of that person unreachable, but it
might be needed in the case of a person who would otherwise
insist on being left out of the family tree.
:eul.

:euserdoc.

