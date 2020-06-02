# SchemeBBS

Anonymous BBS written in MIT Scheme.

## Demo Boards

* [https://textboard.org/prog](https://textboard.org/prog)
* [https://bbs.jp.net/mona](https://textboard.org/prog)

A bit of context: [https://textboard.org](https://textboard.org)


## Run it

```
./create-boards.sh romance smoothies origami
./init.sh 8080
```

SchemeBBS should not directly serve clients, even if it's possible to do so.
The HTTP implementation is far too incomplete and you should use your 
favorite web server as a reverse proxy.

## Patching MIT Scheme

**SchemeBBS web server won't run properly if you don't patch MIT Scheme 9.2!**

The file runtime/http-syntax.scm follows the RFC 2616 which requires
that the value of the Location header be an absolute URI.

The standard has been replaced (see RFC 7231 section 7.1.2.) and a
relative URI is now allowed.

How to apply this patch:

```
curl -O http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/9.2/mit-scheme-9.2.tar.gz
curl -O https://gitlab.com/naughtybits/schemebbs/-/raw/master/patch-runtime_http-syntax.scm
tar xzvf mit-scheme-9.2.tar.gz
patch -p0 < patch-runtime_http-syntax.scm
cd mit-scheme-9.2/src
./configure
make
sudo make install
```

You can also download pre-patched binaries for x86_64:
```
curl -O https://textboard.org/static/mit-scheme-9.2/mit-scheme-9.2b-x86-64.tar.gz
cd mit-scheme-9.2/src
./configure && make
sudo make install
```

## Docker Images

[TeamWau](https://github.com/TeamWau/) published two 
[Docker Images](https://github.com/TeamWau/docker-schemebbs) 
that do everything for you. Thanks to them.

### Standalone SchemeBBS webapp
```
git clone https://github.com/TeamWau/docker-schemebbs.git
cd schemebbs && ./create-boards.sh prog art knitting
export SBBS_DATADIR=/opt/bbs
docker run -p 80:8080 --name sbbs -v "${SBBS_DATADIR}":/opt/schemebbs/data -d erkin/schemebbs
```
### SchemeBBS with pre-configured Nginx
```
git clone https://github.com/TeamWau/docker-schemebbs-nginx.git
cd schemebbs &&./create-boards.sh cats travel food
export SBBS_DATADIR=/opt/bbs
docker run -p 80:80 --name sbbs -d  -v "${SBBS_DATADIR}":/opt/schemebbs/data \
    -v "$(pwd)"/nginx.conf:/opt/nginx/conf/nginx.conf erkin/schemebbs-nginx
```

## Emacs Client

For an improved browsing experience, Anon wrote a praiseworthy Emacs client:
[sbbs.el](https://fossil.textboard.org/sbbs/index) which is taking advantage
of the [S-exp API](https://textboard.org/sexp/prog/). `sbbs.el` is the
recommended gear to ride SchemeBBS' boards, try it.

## Userscripts

There's no Javascript at all in SchemeBBS but you can inject your own if you
need to change the behaviour of the BBS. The HTML is minimalistic, so it's
pretty straightforward. There's a small collection of
[such userscripts here](https://fossil.textboard.org/userscripts/dir?ci=tip).

## License
```
Copyright 2020 Ben Bitdiddle

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
```
