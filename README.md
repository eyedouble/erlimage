# erlimage β
Image processing library implemented as Erlang NIF using FreeImage and Libpngquant

## Introduction

Erlimage is designed as a read, transform, write workflow.
The really cool thing about Erlimage is that the actual pixelbuffer  is returned as Binary to Erlang as well as some metadata. 

You read an image file, apply an arbitrary amount of transformations, and lastly write the end result to disk.



## Api

### I/O
#### readFile/1
```erlang
readFile ( "test.jpg" ).
> {ok, {witdth, 100, height, 100, pitch, 4, bpp, 32, data, <<0,0,0,...>>}}

```

#### writeJpg/4
Filename, Data object, Quality (0-100), Progressive (0,1)
```erlang
writeJpg ( "test.jpg", DataOb, 75, 0 ).
> {ok, "written"}

```

#### writeWebp/4
Filename, Data object, Quality (0-100), Lossless (0,1)
```erlang
writeJpg ( "test.webp", DataOb, 75, 0 ).
> {ok, "written"}

```

#### writePng/
Not implemented yet.

#### writeBmp/
Not implemented yet.

#### writeTiff/
Not implemented yet.

#### writeGif/
Not implemented yet.

#### writeBmp/
Not implemented yet.

#### writeIco/
Not implemented yet.

### Transformations

#### adjustBrightness/
Not implemented yet

#### adjustContrast/
Not implemented yet

#### adjustCurve/
Not implemented yet

#### adjustGamma/
Not implemented yet

#### crop/
Not implemented yet


#### rescale/3,4
Data object, Width, Height, Algorithm (0-5)

Rescale takes three arguments and an optional fourth one. 
The first argument Data object should be populated with a Object return by `readFile/1`.
The second argument is the destination width in pixels. 
The third argument is the destination height in pixels. 
The fourth optional argument can be used to set the resampling algorithm. It defaults to bicubic resampling.

##### Resampling algorithms
- 0: FILTER_BICUBIC Mitchell and Netravali's two-param cubic filter
- 1: FILTER_BOX Box, pulse, Fourier window, 1st order (constant) B-Spline
- 2: FILTER_BILINEAR Bilinear filter
- 3: FILTER_BSPLINE 4th order (cubic) B-Spline
- 4: FILTER_CATMULLROM Catmull-Rom spline, Overhauser spline
- 5: FILTER_LANCZOS3 Lanczos-windowed sinc filter

```erlang
rescale ( DataOb, 500, 500, 0 ).
> {ok, {witdth, 500, height, 500, pitch, 4, bpp, 32, data, <<0,0,0,...>>}}

```

#### rotate/2
DataOb, Degrees
Not implemented yet


### Misc
#### version/0
```erlang
version().
> {ok, "Erlimage:x.x.x;Y:x.x.x;"}

```
