unit      MathStuff;
  { Compute factorials, binomial coefficients, powers for small integers }

  { Copyright (c) 1999-2005 by Tom Verhoeff.

    This file is part of YahtzeeTrainer.  YahtzeeTrainer helps play and analyze
    solitaire games of Yahtzee.

    YahtzeeTrainer is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    YahtzeeTrainer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scronpy; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Author's contact information: <mailto:Tom.Verhoeff@acm.org>.
  }

{$MODE Delphi}{$H+}

interface

  const
    MaxSmallNat = 15;
    Unknown = -1; { Unknown < 0 }

  type
    SmallNat = 0..MaxSmallNat;
    MathTable  = array [ SmallNat ] of longint;
    MathTable2 = array [ SmallNat ] of MathTable;

  function  fac (n: SmallNat): longint;
  { pre: true; ret: n! }

  function  power (n, k: SmallNat): longint;
  { pre: true; ret: n^k }

  function  choose (n, k: SmallNat): longint;
  { pre: true; ret: n choose k }

function  erfc (x: real): real;
function  Phi (x, mu, sigma2: real): real;
procedure TestMathStuff;

implementation
{ Based on recursive definitions and "self-initializing" tables }

  var
    factbl: MathTable;
    choosetbl, powtbl: MathTable2;

  procedure InitMathTable (var t: MathTable);
  { pre: true; post: (A i::t[i]=Unknown) }
    var
      n: SmallNat;
  begin
  for n := 0 to MaxSmallNat do
    t[n] := Unknown;
  end; { InitMathTable }

  procedure InitMathTable2 (var t: MathTable2);
  { pre: true; post: (A i,j::t[i,j]=Unknown) }
    var
      n: SmallNat;
  begin
  for n := 0 to MaxSmallNat do
    InitMathTable(t[n]);
  end; { InitMathTable2 }

  function  fac (n: SmallNat): longint;
  { pre: true; ret: n! }
  begin
  if factbl[n] < 0 then
    if n = 0 then
      factbl[n] := 1
    else { n > 0 }
      factbl[n] := n * fac(n - 1);
  fac := factbl[n];
  end; { fac }

  function  power (n, k: SmallNat): longint;
  { pre: true; ret: n^k }
  begin
  if powtbl[n, k] < 0 then
    if k = 0 then
      powtbl[n, k] := 1
    else { k > 0 }
      if odd(k) then
        powtbl[n, k] := n * sqr(power(n, k div 2))
      else
        powtbl[n, k] := sqr(power(n, k div 2));
  power := powtbl[n, k];
  end; { power }

  function  choose (n, k: SmallNat): longint;
  { pre: true; ret: n choose k }
  begin
  if choosetbl[n, k] < 0 then
    if k > n then
      choosetbl[n, k] := 0
    else if (k = 0) or (k = n) then
      choosetbl[n, k] := 1
    else { 0 < k < n }
      choosetbl[n, k] := choose(n - 1, k - 1) + choose(n - 1, k);
  choose := choosetbl[n, k];
  end; { choose }

function  erfc (x: real): real;
  { pre: true
    ret: complementary error function erfc(x) with fractional error less
         than 1.2e-7, according to Numerical Recipes in Pascal }
  var t, z, ans: real;
  begin
    z := abs(x)
  ; t := 1.0 / (1.0 + 0.5*z)
  ; ans := t*exp(-sqr(z) - 1.26551223
                     + t*( 1.00002368
                     + t*( 0.37409196
                     + t*( 0.09678418
                     + t*(-0.18628806
                     + t*( 0.27886807
                     + t*(-1.13520398
                     + t*( 1.48851587
                     + t*(-0.82215223
                     + t*  0.17087277)))))))))
  ; if x >= 0.0 then erfc := ans
    else erfc := 2.0 - ans
  end; { erfc }

function  Phi (x, mu, sigma2: real): real;
  { pre: 0 <= sigma2
    ret: Pr(N <= x) where N is normally distributed with parameter mu, sigma2 }
  begin
    Phi := 1 - 0.5 * erfc((x-mu)/sqrt(2*sigma2))
  end; { Phi }

procedure TestMathStuff;
  var k: integer;
  begin
    for k := 0 to 6 do
      writeln('Phi(', k/2:3:1, ') = ', Phi(k/2, 0.0, 1.0):7:5)
  end; { TestMathStuff }

begin

  InitMathTable(factbl);
  InitMathTable2(choosetbl);
  InitMathTable2(powtbl);

end. { unit MathStuff }
