This project is for my personal use to upload Quicklisp client files
in versioned way.

Quicklisp client files involve three bits:

  - quicklisp.tar, a tar archive containing the lisp code that
    implements the parts of Quicklisp that can search in the Quicklisp
    dist indexes and download & unpack source code

  - setup.lisp, a file that is loaded in your implementation to get
    the quicklisp client code loaded

  - asdf.lisp, a file that implements a system definition facility

quicklisp-client-uploader checks the quicklisp-client repo directory
for those three pieces and uploads uploads them to Amazon S3. 

Pieces that are unchanged since some previously uploaded version are
not uploaded again; for example, if two versions both use ASDF 43.2,
they will point to the same ASDF 43.2 URL instead of uploading ASDF
again each time.

It also uploads gzipped versions of those pieces, too.

It's not meant to be generally useful, and is not written in anything
that I'd consider imitable or didactic style.

To use, update & commit version.txt in the quicklisp client directory,
make sure everything is clean via "make clean", remove or commit any
untracked files, and use (publish-client
"/path/to/quicklisp-client/"). Publishing quicklisp.lisp is a matter
of (upload-quicklisp-lisp "/path/to/quicklisp-bootstrap/").

Zach



