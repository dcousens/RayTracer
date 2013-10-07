RayTracer
=========

A Haskell implementation of Paul Heckbert's business card ray tracer.

## How to build?
If you don't want to be waiting for several minutes, your best bet is to ensure `-O` flag is enabled when compiling.

`ghc -O Tracer.hs`

## How to run?
`./Tracer > output.ppm`

## Output
![example][1]

## Known Issues
This rendition of the ray tracer does not do any multi-sampling (yet) or lambert illumination of the ground tiling.

## Credit
This source is based on [Fabiens](http://fabiensanglard.net/rayTracing_back_of_business_card/index.php) breakdown of Paul Heckbert's original code.

Special thanks to `#haskell` for helping me with my million questions.


  [1]: http://i.imgur.com/11XBtCx.png
