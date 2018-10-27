# SchemeBBS

The file runtime/http-syntax.scm follows the RFC 2616 which requires
that the value of the Location header be an absolute URI.

The standard has been replaced (see RFC 7231 section 7.1.2.) and a
relative URI is now permitted.

How to apply this patch:

```
curl -O http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/9.2/mit-scheme-9.2.tar.gz
patch -s -p0 mit-scheme-9.2.patch
cd mit-scheme-9.2/src
make
sudo make install
```

