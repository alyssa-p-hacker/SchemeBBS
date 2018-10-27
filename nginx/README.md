Nginx needs to be compiled with these modules:

* https://github.com/yaoweibin/ngx_http_substitutions_filter_module
* https://github.com/aperezdc/ngx-fancyindex

```
 ./configure --add-module=modules/ngx_http_substitutions_filter_module\
             --add-module=modules/ngx-fancyindex
 ```
