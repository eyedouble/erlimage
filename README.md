# erlimage β
Image processing library implemented as Erlang NIF using FreeImage and Libpngquant

## Introduction

Erlimage is designed as a read, transform, write workflow.
The really cool thing about Erlimage is that the actual pixelbuffer  is returned as Binary to Erlang as well as some metadata. 

You read an image file, apply an arbitrary amount of transformations, and lastly write the end result to disk.

## Known to work on:
- Ubuntu 17.10
- Ubuntu 16.04
- Fedora 27
- Windows 10 x64 (Compile via MinGW gcc) *See usage on Windows section below*

## Prerequisites
- gcc
- g++

*Please note that that Windows is only supported with Erlang/OTP 21.0 and higher. Since this release the folder containing the nif's dll is automatically added to the dll search path (OTP-14666). Erlimage uses this functionality to load it's depencency.*

*Please note on fedora and possibly other linux distro's that use yum as package manger the g++ compiler is called gcc-c++*

## Install
Add `{erlimage, {git, "https://github.com/eyedouble/erlimage.git", {tag, "X.Y.Z-Z1"}}}` to the `rebar.config` file of your application and add `erlimage` to your `application.src` file. Where X.Y.Z-Z1 is the release tag. See the releases tab for the latest release.


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




## Usage on Windows 
To build erlimage on Windows 64bit you need to provide GNU gcc and g++ compilers.
Follow the steps below:

1. Download  [MinGW64 for Windows 64bit](http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.9.2/threads-win32/seh/x86_64-4.9.2-release-win32-seh-rt_v3-rev0.7z/download) , and unzip to `C:\mingw64`.

2. Add `C:\mingw64\bin` to your Path environment variable. ( On Windows 10: Open the Start menu, type `environment` in the results click on `Edit environment variables for your account`. Select the `Path` entry and click on `Edit`. )

3. Open CMD or Powershell and execute `gcc --version`. It should show you the gcc version that is present. Do the same for g++. You are now done, please see the section **Install** to continue.

4. Note that the latest version of Erlimage on Windows is only compatible with Erlang/OTP 21 or higher.
