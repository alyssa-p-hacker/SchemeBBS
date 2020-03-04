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

Note: SchemeBBS should not directly serve clients, its HTTP implementation is 
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
tar xzvf mit-scheme-9.2.tar.gz
cd mit-scheme-9.2
patch -p0 < mit-scheme-9.2.patch
cd mit-scheme-9.2/src
./configure
make
sudo make install
```

## License

SchemeBBS is an anonymous textboard software (2ch clone)
Copyright (C) 2020 Ben Bitdiddle

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see [https://www.gnu.org/licenses/agpl-3.0.en.html](gnu.org/licenses/agpl-3.0).
