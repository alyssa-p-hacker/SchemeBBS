# SchemeBBS

Anonymous BBS written in MIT Scheme

## Demo

[https://textboard.org/prog](http://textboard.org/prog)
[https://textboard.org](http://textboard.org)

You can also access it anonymously via i2p

[http://7ubwrcixdcemzqwqzh2vaakjsnochj2biuzpo6dc2n4f7wqj4pua.b32.i2p](http://textboard.i2p)

## Run it

```
./init.sh 8080
```

Note: SchemeBBS should not directly serves client, its HTTP implementation is 
far too incomplete. It needs Nginx as a reverse proxy for caching and serving
static files.

## Patching MIT Scheme

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

