# erlimage
Image processing library implemented as Erlang NIF using FreeImage and Libpngquant

## Introduction

Erlimage is designed as a read, transform, write workflow.
The really cool thing about Erlimage is that the actual pixelbuffer  is returned as Binary to Erlang as well as some metadata. 

You read an image file, apply an arbitrary amount of transformations, and lastly write the end result to disk.



## Api

### readFile/1
```erlang
readFile ( "test.jpg" ).
> {ok, {witdth, 100, height, 100, pitch, 4, bpp, 32, data, <<0,0,0,..,>>}}

```
