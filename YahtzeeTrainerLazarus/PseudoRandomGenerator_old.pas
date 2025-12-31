unit      PseudoRandomGenerator;

{ (c) Copyright 1999-2000, Tom Verhoeff }

interface

  { The Macintosh Random routine is based on
    the "standard" generator described in:
      S.K. Park & K.W. Miller,
      "Random number generators: Good ones are hard to find",
      CACM, Vol. 31, pp. 1192--1201. }
  { Random kicks the generator once,
    i.e. computes  randSeed := (a*randSeed) mod m . }
  {	multiplier: a = 16807; }
  { modulus:    m = 2147483647; }
  { The return value of Random is derived from randSeed. }

const
  InitialRandSeed = 1134512352;

{ The following two routines use an external C library for the
  "Mersenne Twister, a 623-dimensionally equidistributed
  Pseudo-Random Number Generator".  See the paper by Makoto Matsumoto
  and Takuji Nishimura in ACM Transactions on Modeling and Computer
  Simulations, Vol. 8, No. 1, January 1998, pp. 3--30. }

{procedure sgenrand (seed: integer); external c; { Sun Pascal }
{function  genrand1_6: integer; external c; { Sun Pascal }
{function  genrand_mod (m: integer): integer; external c; { Sun Pascal }

{x$L mt19937int-tom.c}
//procedure sgenrand (seed: {MedCard}integer); Cdecl; { gpc }
//function  genrand1_6: integer; Cdecl; { gpc }
//function  genrand_mod (m: integer): integer; Cdecl; { gpc }

procedure InitRandom (seed: integer);

implementation 

{ Sun Pascal version }
{procedure InitRandom (newSeed: longint);}
{  var oldSeed: longint;}
{  begin}
{    oldSeed := seed(newSeed)}
{  end; { InitRandom }

procedure InitRandom (seed: integer);
  begin
{   sgenrand(seed) { gpc }
    RandSeed := seed { Delphi }
  ; writeln('Seed for Random Number Generater set to ', seed:10)
  end; { InitRandom }

end. { unit PseudoRandomGenerator }
