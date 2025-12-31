unit      DiceStuff;
  { Dice, sets of dice, fast traversal of all possible lists/bags of values }
  { Still has some magic (unnamed) constants (to be named in the future) }

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

uses
  GeneralStuff;

const
  MinValue = 1; { lowest value on a die }
  MaxValue = 6; { highest value on a die }
  NValues = MaxValue - MinValue + 1; { number of values on a die }
  NDice = 5; { number of dice (typicall for Yahtzee) }
  MaxValueMultiplicity = NDice;
    { # ValueLists of size k = NValues^k }
    { # ValueLists of size at most k = NValues^(k+1) div (NValues-1) }
    {                         k: 0  1   2    3     4     5 }
    { # ValueLists of size  = k: 1  6  36  216  1296  7776 }
    { # ValueLists of size <= k: 1  7  43  259  1555  9331 }
    { There is a 1-1 correspondence between ascending ValueLists and ValueBags }
    { # ValueBags of size k = NValues+k-1 choose k }
    { # ValueBags of size at most k = NValues+k choose k }
    {                        k : 0  1   2   3     4     5 }
    { # ValueBags of size  = k : 1  6  21  56   126   252 }
    { # ValueBags of size <= k : 1  7  28  84   210   462 }
  EmptyValueListRank = 0;
  EmptyValueBagRank = 0;
  MaxValueListRank = pred(9331);
  NValueBag4Ranks = 210; { = # ValueBags with size<NDice }
  NValueBag5Ranks = 462; { = # ValueBags with size<=NDice }
  MaxValueBagRank = pred(NValueBag5Ranks);

type
  DieValue = MinValue..MaxValue; { values on a die }
    { DieValue used to be called Value, but that is a reserved word for gpc }
  DiceCount = 0..NDice;
  DiceIndex = 1..NDice;

  DiceSet = set of DiceIndex;
  ValueList = record
      siz: DiceCount;
      val: array [ DiceIndex ] of DieValue; { val[1..siz] is defined }
    end; { record }

  ValueListRank = EmptyValueListRank..MaxValueListRank;

  ValueMultiplicity = 0..MaxValueMultiplicity;

  ValueBag = array [ DieValue ] of ValueMultiplicity;
    { all that matters of a dice roll in Yahtzee is the ValueBag }
    { repr.inv.: For vb: ValueBag, (+ i:: vb[i]) <= NDice, i.e. size<=NDice }

  ValueBagRank = EmptyValueBagRank..MaxValueBagRank;
  ValueBag5Rank = NValueBag4Ranks..MaxValueBagRank;
    { Ranks for VBs with size=NDice }

  Probability = real; { actually 0.0 .. 1.0 }

  ValueSubBagIndex = 0..4367;
  ValueBagCompletionIndex = 0..4367;

var {const, initialized by InitDiceStuff }
  EmptyValueList: ValueList;
  EmptyValueBag: ValueBag;

  ValueListTbl: array [ ValueListRank ] of record
    fvl: ValueList;
    sz: DiceCount;
    vbr: ValueBagRank;
  end; { record }
  { For ValueListTbl[r],
      fvl = ValueList with rank r
      sz  = length of fvl
      vbr = rank of corresponding ValueBag }

  ValueListRanks: array [ DiceCount ] of record
      firstvlr, lastvlr: ValueListRank;
      nvl: integer; { # ValueList of given size }
    end; { record }
    { With ValueListRanks[s]:
        ValueListTbl[firstvlr..lastvlr].fvl =
          list of all ValueLists with size s,
          lexicographically ascending }

  ValueBagTbl: array [ ValueBagRank ] of record
      fvb: ValueBag; { ValueBagTbl[r].fvb = ValueBag with rank r }
      sz: ValueMultiplicity; { ValueBagSize(fvb) }
      pr: Probability; { probability of rolling fvb with sz fair dice }
      vlr: ValueListRank; { rank of corresponding ascending ValueList }
    end; { record }

  ValueBagRanks: array [ ValueMultiplicity ] of record
      firstvbr, lastvbr: ValueBagRank;
      nvb: integer; { # ValueBags of given size }
    end; { record }
    { With ValueBagRanks[s]:
        ValueBagTbl[firstvbr..lastvbr].fvb = list of all ValueBags with size s,
          lexicographically ascending }

  ValueSubBagIndices: array [ ValueBag5Rank ] of record
      firstvsbi, lastvsbi: ValueSubBagIndex;
    end; { record }
  ValueSubBags: array [ ValueSubBagIndex ] of record
      sr: ValueBagRank;
    end; { record }
  { With ValueSubBagIndices[r]:
      ValueSubBags[firstvsbi..lastvsbi].sr = list of ranks of all subbags of r,
        lexicographically ascending }

  ValueBagCompletionIndices: array [ ValueBagRank ] of record
      firstvbci, lastvbci: ValueBagCompletionIndex;
    end; { record }
  ValueBagCompletions: array [ ValueBagCompletionIndex ] of record
      cpr: Probability;
      sum: ValueBag5Rank;
    end; { record }
  { With ValueBagCompletionIndices[r]:
      ValueBagCompletions[firstvbci..lastvbci] = list of probability and rank of
        all completions to size NDice of r,
        lexicographically ascending by subbag added to r }

  RankFromValueBagTbl: array
    [ ValueMultiplicity, ValueMultiplicity, ValueMultiplicity,
      ValueMultiplicity, ValueMultiplicity, ValueMultiplicity ] of ValueBagRank;
    { For vb: ValueBag,
        RankFromValueBagTbl[vb[1],vb[2],vb[3],vb[4],vb[5],vb[6]] = rank of vb }
    { Overkill by a factor 100 in size: 6^6 = 46656 versus N5Ranks=462 }

procedure ReadValue (var f: text; var {out} v: DieValue);

function  StringFromValueList (vl: ValueList; wide: boolean): stdstring;
procedure WriteValueList (var f: text; s: stdstring; vl: ValueList);
procedure MakeEmptyValueList (var {out} vl: ValueList);
procedure ExtendValueList (var vl: ValueList; v: DieValue; cnt: DiceCount);
procedure ReadValueList (var f: text; prompt: stdstring;
                         var {out} vl: ValueList);
procedure FirstValueList (var {out} vl: ValueList; s: DiceCount);
function  NextValueList (var vl: ValueList): boolean;
function  RankFromValueList (vl: ValueList): ValueListRank;

procedure ValueListFromValueBag (var {out} vl: ValueList; vb: ValueBag);
function  StringFromValueBag (vb: ValueBag; wide: boolean): stdstring;
function  StringFromValueBagRel (vb: ValueBag; vl: ValueList): stdstring;
procedure WriteValueBag (var f: text; s: stdstring; vb: ValueBag);
function  IsEqualValueBag (va, vb: ValueBag): boolean;
function  IsSubValueBag (va, vb: ValueBag): boolean;
function  ValueBagSupport (vb: ValueBag): integer;
function  ValueBagSize (vb: ValueBag): integer;
function  ValueBagWeight (vb: ValueBag): integer;
function  ValueBagMinValue (vb: ValueBag): integer;
function  ValueBagMaxValue (vb: ValueBag): integer;
function  ValueBagMinNonZeroMult (vb: ValueBag): integer;
function  ValueBagMaxMult (vb: ValueBag): ValueMultiplicity;
function  ValueBagMaxInterval (vb: ValueBag): integer;
procedure MakeEmptyValueBag (var {out} vb: ValueBag);
procedure ExtendValueBag (var vb: ValueBag; v: DieValue; m: ValueMultiplicity);
procedure AddValueBag (var {out} va: ValueBag; vb, vc: ValueBag);
procedure ValueBagFromValueList (var {out} vb: ValueBag; vl: ValueList);
procedure ReadValueBag (var f: text; prompt: stdstring; var vb: ValueBag);
procedure FirstValueBag (var {out} vb: ValueBag; s: ValueMultiplicity);
function  NextValueBag (var vb: ValueBag): boolean;
procedure FirstValueSubBag (var {out} vb: ValueBag);
function  NextValueSubBag (var vb: ValueBag; vc: ValueBag): boolean;
function  RankFromValueBag (vb: ValueBag): ValueBagRank;
function  ValueBagProbability (vb: ValueBag): Probability;

implementation

uses
  MathStuff;

procedure ReadValue (var f: text; var {out} v: DieValue);
  var ch: char;
  begin
  ; ReadChar(f, ch)
  ; v := DigitFromChar(ch)
  end; { ReadValue }

function  StringFromValueList (vl: ValueList; wide: boolean): stdstring;
  var
    d: DiceCount; { actually DiceIndex, but Sun Pascal then fails when siz=0 }
    s: stdstring;
  begin
    with vl do begin
      s := ''
    ; for d := 1 to siz do begin
        s := s + StringFromInt(val[d], 1)
      ; if wide then s := s + ' '
      end { for d }
    end { with vl }
  ; StringFromValueList := s
  end; { StringFromValueList }

procedure WriteValueList (var f: text; s: stdstring; vl: ValueList);
  var i: DiceCount; { actually DiceIndex, but Sun Pascal then fails if siz=0 }
  begin
    write(f, s)
  ; with vl do begin
      for i := 1 to siz do
        write(f, val[i]:2)
    ; writeln(f)
    end { with vl }
  end; { WriteValueList }

procedure MakeEmptyValueList (var {out} vl: ValueList);
  { pre: true; post: vl = [ ] }
  begin
    with vl do
      siz := 0
  end; { MakeEmptyValueList }

procedure ExtendValueList (var vl: ValueList; v: DieValue; cnt: DiceCount);
  { pre:  vl~.siz + cnt~ <= NDice
    post: vl = vl~ ++ [v]^cnt~ }
  begin
  ; with vl do begin
      if not(siz+cnt <= NDice) then Fail('ExtendValueList', 'siz+cnt <= NDice')
    ; while cnt <> 0 do begin
        siz := succ(siz)
      ; val[siz] := v
      ; cnt := pred(cnt)
      end { while }
    end { with vl }
  end; { ExtendValueList }

procedure ReadValueList (var f: text; prompt: stdstring;
                         var {out} vl: ValueList);
  var v: DieValue;
  begin
    with vl do begin
      if prompt <> '' then
        write(prompt)
    ; MakeEmptyValueList(vl)
    ; while not EndOfField(f) do begin
        ReadValue(f, v)
      ; ExtendValueList(vl, v, 1)
      end { while }
    ; ReadEndOfField(f)
    end { with vl }
  end; { ReadValueList }

procedure FirstValueList (var {out} vl: ValueList; s: DiceCount);
  { pre:  true
    post: vl = first ValueList of size s~ in lexicographic ascending order } 
  begin
    vl := EmptyValueList
  ; ExtendValueList(vl, MinValue, s)
    { vl.siz=s~ /\ (A i: 1<=i<=s~: vl.val[i]=MinValue) }
  end; { FirstValueList }

function  NextValueList (var vl: ValueList): boolean;
  { pre:  true
    post: vl = asc. lex. successor of vl~ with same size, if such exists
    ret:  vl is indeed lexicographic successor }
  var
    i, j: integer; { actually: [0]+DiceIndex (viz. for last vl) }
  begin
    with vl do begin
      i := 0
    ; j := siz
      { Bounded Linear Search for greatest j with val~[j] < MaxValue,
        side-effect: skipped values with val~[j]=MaxValue set to MinValue }
    ; while i <> j do begin
        if val[j] < MaxValue then begin
          val[j] := succ(val[j])
        ; i := j
        end { then }
        else { val[j] = MaxValue } begin
          val[j] := MinValue
        ; j := pred(j)
        end { else }
      end { while }
    ; NextValueList := i <> 0
    end { with vl }
  end; { NextValueList }

function  RankFromValueList (vl: ValueList): ValueListRank;
  { ret: rank of vl~ among all ValueLists, sorted on size then lex. asc. }
  var
    offset: integer;
    i: DiceCount; { actually DiceIndex, but then Sun Pascal fails for siz=0 }
  begin
    with vl, ValueListRanks[siz] do begin
      offset := 0
    ; for i := 1 to siz do begin
        offset := NValues * offset + val[i] - 1
        { N.B. Sun Pascal fails on pred(val[i]) for val[i]=MinValue }
      end { for i }
    ; RankFromValueList := firstvlr + offset
    end { with vl }
  end; { RankFromValueList }

procedure ValueListFromValueBag (var {out} vl: ValueList; vb: ValueBag);
  { post: vl = vb~ as ascending list }
  var v: DieValue;
  begin
    vl := EmptyValueList
  ; for v := MinValue to MaxValue do
      ExtendValueList(vl, v, vb[v])
  end; { ValueListFromValueBag }

function  StringFromValueBag (vb: ValueBag; wide: boolean): stdstring;
  var s: stdstring; v: DieValue; i, size: ValueMultiplicity;
  begin
    s := ''
  ; size := 0
  ; for v := MinValue to MaxValue do begin
      for i := 1 to vb[v] do begin
        s := s + CharFromDigit(v)
      ; if wide then
          s := s + ' '
      end { for i }
    ; size := size + vb[v]
    end { for v }
  ; while size <> NDice do begin
      s := s + '_'
    ; if wide then
        s := s + ' '
    ; size := succ(size)
    end { while }
  ; StringFromValueBag := s
  end; { StringFromValueBag }

function  StringFromValueBagRel (vb: ValueBag; vl: ValueList): stdstring;
  { pre: vb~ subbag of vl~ }
  { ret: bag vb~ as string relative to roll vl~ }
  { N.B. vb cannot be var param because it is locally modified }
  var
    d: DiceCount; { actually DiceIndex, but Sun Pascal then fails when siz=0 }
    s: stdstring;
  begin
    with vl do begin
      s := ''
    ; for d := 1 to siz do begin
        if vb[val[d]] = 0 then
          s := s + '_ '
        else begin
          s := s + StringFromInt(val[d], 1) + ' '
        ; vb[val[d]] := pred(vb[val[d]])
        end { else }
      end { for d }
    end { with vl }
  ; StringFromValueBagRel := s
  end; { StringFromValueBagRel }

procedure WriteValueBag (var f: text; s: stdstring; vb: ValueBag);
  var sep: stdstring; i: DieValue;
  begin
    write(f, s)
  ; sep := ''
  ; for i := MinValue to MaxValue do begin
      if vb[i] = 0 then
        write(f, sep, '___ _')
      else
        write(f, sep, '[', i:1, '] ', vb[i]:1)
    ; sep := ', '
    end { for i }
  ; writeln(f)
  end; { WriteValueBag }

function  IsEqualValueBag (va, vb: ValueBag): boolean;
  { pre: true; ret: va~ = vb~ }
  var i, j: integer; { actually DieValue+[succ(MaxValue)] }
  begin
    i := MinValue ; j := succ(MaxValue)
    { Bounded Linear Search for least violation of equality }
  ; while i <> j do
      if va[i] = vb[i] then i := succ(i)
      else j := i
  ; IsEqualValueBag := i > MaxValue
  end; { IsEqualValueBag }

function  IsSubValueBag (va, vb: ValueBag): boolean;
  { pre: true; ret: va~ <= vb~ }
  var i, j: integer; { actually DieValue+[succ(MaxValue)] }
  begin
    i := MinValue ; j := succ(MaxValue)
    { Bounded Linear Search for least violation of subbag containment }
  ; while i <> j do
      if va[i] <= vb[i] then i := succ(i)
      else j := i
  ; IsSubValueBag := i > MaxValue
  end; { IsSubValueBag }

function  ValueBagSupport (vb: ValueBag): integer;
  { pre: true
    ret: (# i:: vb~[i]<>0) }
  var
    i: DieValue;
    s: integer;
  begin
    s := 0
  ; for i := MinValue to MaxValue do
      s := s + ord(vb[i]<>0)
  ; ValueBagSupport := s
  end; { ValueBagSupport }

function  ValueBagSize (vb: ValueBag): integer;
  { pre: true
    ret: (+ i:: vb~[i]) }
  var
    i: DieValue;
    s: integer;
  begin
    s := 0
  ; for i := MinValue to MaxValue do
      s := s + vb[i]
  ; ValueBagSize := s
  end; { ValueBagSize }

function  ValueBagWeight (vb: ValueBag): integer;
  { pre: true
    ret: (+ i:: i*vb~[i]) }
  var
    i: DieValue;
    s: integer;
  begin
    s := 0
  ; for i := MinValue to MaxValue do
      s := s + i*vb[i]
  ; ValueBagWeight := s
  end; { ValueBagWeight }

function  ValueBagMinValue (vb: ValueBag): integer;
  { return type actually DieValue+[succ(MaxValue)] }
  { pre: true
    ret: (min i: vb~[i]<>0: i) if vb~ not empty else succ(MaxValue) }
  var
    i, j: integer; { actually DieValue+[succ(MaxValue)] }
  begin
    i := MinValue
  ; j := succ(MaxValue) { Bounded Linear Search for smallest i with vb[i]<>0 }
  ; while i<>j do
      if vb[i]=0 then i := succ(i)
      else j:=i
  ; ValueBagMinValue := i
  end; { ValueBagMinValue }

function  ValueBagMaxValue (vb: ValueBag): integer;
  { return type actually [pred(MinValue)]+DieValue }
  { pre: true
    ret: (max i: vb~[i]<>0: i) if vb~ not empty else pred(MinValue) }
  var
    i, j: integer; { actually [pred(MinValue)]+DieValue }
  begin
    i := pred(MinValue)
  ; j := MaxValue { Bounded Linear Search for greatest j with vb[j]<>0 }
  ; while i<>j do
      if vb[j]=0 then j := pred(j)
      else i:=j
  ; ValueBagMaxValue := j
  end; { ValueBagMaxValue }

function  ValueBagMinNonZeroMult (vb: ValueBag): integer;
  { return type actually ValueMultiplicity + [succ(MaxValueMultiplicity)] }
  { pre: true
    ret: (min i: vb~[i]<>0: vb~[i]) if vb~ not empty
         else succ(MaxValueMultiplicity) }
  var
    i: DieValue;
    s: integer;
  begin
    s := succ(MaxValueMultiplicity)
  ; for i := MinValue to MaxValue do
      if (vb[i]<>0) and (vb[i]<s) then s := vb[i]
  ; ValueBagMinNonZeroMult := s
  end; { ValueBagMinNonZeroMult }

function  ValueBagMaxMult (vb: ValueBag): ValueMultiplicity;
  { pre: true
    ret: (max i:: vb~[i]) }
  var
    i: DieValue;
    s: ValueMultiplicity;
  begin
    s := 0
  ; for i := MinValue to MaxValue do
      if vb[i]>s then s := vb[i]
  ; ValueBagMaxMult := s
  end; { ValueBagMaxMult }

function  ValueBagMaxInterval (vb: ValueBag): integer;
  { pre: true
    ret: (max i,j: MinValue<=i<=j<=MaxValue /\ NZS.i.j: j-i+1),
         where NZS.i.j = (A k: i<=k<=j: vb~[k]<>0) }
  { returns length of longest segment of non-zero multiplicities in vb~,
    i.e., length of longest contiguous sequence of numbers in bag }
  var
    v: DieValue;
    m, s: integer; { actually: 0..MaxValue-MinValue+1 }
  begin
    m := 0 ; s := 0
    { inv: m = (max i,j: MinValue<=i<=j<v /\ NZS.i.j: j-i+1)
           s = (max i: MinValue<=i<v /\ NZS.i.v: v-i+1)  "longest NZ tailseg" }
  ; for v := MinValue to MaxValue do
      if vb[v]<>0 then begin
        s := succ(s)
      ; if s > m then m := s
      end { then }
      else
        s := 0
  ; ValueBagMaxInterval := m
  end; { ValueBagMaxInterval }

procedure MakeEmptyValueBag (var {out} vb: ValueBag);
  { pre: true; post: vb = [ ] }
  var i: DieValue;
  begin
    for i := MinValue to MaxValue do
      vb[i] := 0
  { (A i:: vb[i]=0) }
  end; { MakeEmptyValueBag }

procedure ExtendValueBag (var vb: ValueBag; v: DieValue; m: ValueMultiplicity);
  { pre:  #vb~ + m~ <= NDice
    post: vb = vb~ + m*[v] }
  begin
    vb[v] := vb[v] + m
  end; { ExtendValueBag }

procedure AddValueBag (var {out} va: ValueBag; vb, vc: ValueBag);
  { pre:  #vb~ + #vc~ <= NDice
    post: va = vb~ + vc~ }
  var v: DieValue;
  begin
    for v := MinValue to MaxValue do
      va[v] := vb[v] + vc[v]
  end; { AddValueBag }

procedure ValueBagFromValueList (var {out} vb: ValueBag; vl: ValueList);
  { pre: true; post: vb = ValueBag of vl~ }
  var i: DiceCount; { actually DiceIndex }
  begin
    with vl do begin
      vb := EmptyValueBag
    ; for i := 1 to siz do
        ExtendValueBag(vb, val[i], 1)
    end { with vl }
  end; { ValueBagFromValueList }

procedure ReadValueBag (var f: text; prompt: stdstring; var vb: ValueBag);
  { read ValueBag vb from input as list }
  var vl: ValueList;
  begin
    ReadValueList(f, prompt, vl)
  ; ValueBagFromValueList(vb, vl)
  end; { ReadValueBag }

procedure FirstValueBag (var {out} vb: ValueBag; s: ValueMultiplicity);
  { pre: true
    post: vb = first ValueBag of size s~ in ascending lexicographic order } 
  begin
    vb := EmptyValueBag
  ; ExtendValueBag(vb, MaxValue, s)
    { (A i: MinValue<=i<MaxValue: vb[i]=0) /\ vb.[MaxValue]=s~ }
  end; { FirstValueBag }

function  NextValueBag (var vb: ValueBag): boolean;
  { pre:  true
    post: vb = asc. lex. successor of vb~ with same size, if such exists
    ret:  vb is indeed lexicographic successor }
  var
    j: integer; { actually: [pred(MinValue)]+DieValue (viz. for empty vb) }
    i: integer; { acutally DieValue, Sun Pascal then fails at "for i:=j" }
    m: ValueMultiplicity;
  begin
    j := ValueBagMaxValue(vb)
  ; NextValueBag := j > MinValue
  ; if j > MinValue then begin
      vb[pred(j)] := succ(vb[pred(j)])
{;writeln('j = ', j:1)}
    ; m := pred(vb[j])
{;writeln('m = ', m:1)}
    ; for i := j to pred(MaxValue) do
        vb[i] := 0
    ; vb[MaxValue] := m
    end { if }
  end; { NextValueBag }

procedure FirstValueSubBag (var {out} vb: ValueBag);
  { pre: true;
    post: vb = first subbag in asc. lexicographic order, i.e. the empty bag } 
  begin
    vb := EmptyValueBag
    { vb = [ ] }
  end; { FirstValueSubBag }

function  NextValueSubBag (var vb: ValueBag; vc: ValueBag): boolean;
  { pre:  vb~ subbag of vc~
    post: vb = asc. lex. successor of vb~ among subbags of vc~, if such exists
    ret:  vb is indeed lexicographic successor }
  var
    i, j: integer; { actually [pred(MinValue)]+DieValue }
  begin
    i := pred(MinValue)
  ; j := MaxValue { Bounded Linear Search for greatest j with vb[j]<vc[j] }
  ; while i<>j do
      if vb[j]=vc[j] then j := pred(j)
      else i:=j
  ; NextValueSubBag := j >= MinValue
  ; if j >= MinValue then begin
      vb[j] := succ(vb[j])
    ; for i := succ(j) to MaxValue do
        vb[i] := 0
    end { if }
  end; { NextValueSubBag }

function  RankFromValueBag (vb: ValueBag): ValueBagRank;
  { ret: rank of vb~ among all ValueBags, sorted on size then lex. asc. }
  begin
    RankFromValueBag := RankFromValueBagTbl[vb[1],vb[2],vb[3],vb[4],vb[5],vb[6]]
  end; { RankFromValueBag }

function  ValueBagProbability (vb: ValueBag): Probability;
  { ret: probability to roll vb~ with fair dice }
  begin
    ValueBagProbability := ValueBagTbl[RankFromValueBag(vb)].pr
  end; { ValueBagProbability }

procedure InitValueListTables;
  { pre:  RankFromValueBagTbl defined (for RankFromValueBag) }
  { post: ValueListTbl, ValueListRanks defined }
  var
    s: DiceCount;
    vl: ValueList;
    vb: ValueBag;
    r: integer; { actually ValueListRank + [succ(MaxValueListRank)] }
  begin
    r := EmptyValueListRank
  ; for s := 0 to NDice do begin
      with ValueListRanks[s] do begin
        firstvlr := r
      ; FirstValueList(vl, s)
      ; repeat
          with ValueListTbl[r] do begin
          ; fvl := vl
          ; if not(RankFromValueList(vl) = r) then Fail('InitValueListTables',
                  'RankFromValueList(vl) = r')
          ; sz := s
          ; ValueBagFromValueList(vb, vl)
          ; vbr := RankFromValueBag(vb)
          end { with }
        ; r := succ(r)
        until not NextValueList(vl)
      ; lastvlr := pred(r)
      ; nvl := r - firstvlr
      ; if not(nvl = power(NValues, s)) then Fail('InitValueListTables',
              'nvl = power(NValues, s)')
      end { with }
    end { for s }
  ; if not(r = succ(MaxValueListRank)) then Fail('InitValueListTables',
          '#ValueLists = succ(MaxValueListRank)')
  end; { InitValueListTables }

procedure InitValueBagTables;
  { pre:  true }
  { post: ValueBagTbl (except field vlr), ValueBagRanks, and
          RankFromValueBagTbl defined }
  var
    s: ValueMultiplicity;
    vb: ValueBag;
    vl: ValueList;
    r: integer; { actually ValueBagRank + [N5ValueBagRanks] }
    h: Probability;
  begin
    r := EmptyValueBagRank
  ; for s := 0 to NDice do begin
      with ValueBagRanks[s] do begin
        firstvbr := r
      ; h := 0.0 { h = (+ B: #B=s /\ rank(B)<r : Pr(B)) }
      ; FirstValueBag(vb, s)
      ; repeat
          RankFromValueBagTbl[vb[1], vb[2], vb[3], vb[4], vb[5], vb[6]] := r
        ; with ValueBagTbl[r] do begin
          ; fvb := vb
          ; sz := s
          ; pr := (fac(s) div ( fac(vb[1]) * fac(vb[2]) * fac(vb[3])
                              * fac(vb[4]) * fac(vb[5]) * fac(vb[6]) ))
                  / power(6, s)
          ; h := h + pr
          ; ValueListFromValueBag(vl, vb) { can be done cheaper, but who cares }
            { vlr will be defined in InitRestValueBagTables }
          end { with }
        ; r := succ(r)
        until not NextValueBag(vb)
      ; lastvbr := pred(r)
      ; nvb := r - firstvbr
      ; if not(nvb = choose(pred(NValues+s), s)) then Fail('InitValueBagTables',
              'nvl = choose(pred(NValues+s), s)')
      end { with }
    ; if not(abs(1.0 - h) < 1e-6) then Fail('InitValueBagTables',
            'Sum of probabilities = 1')
    end { for s }
  ; if not(r = NValueBag5Ranks) then Fail('InitValueBagTables',
          '#ValueBags = NValueBag5Ranks')
  ; if not(ValueBagRanks[5].firstvbr = NValueBag4Ranks) then
      Fail('InitValueBagTables',
          'ValueBagRanks[5].firstvbr = NValueBag4Ranks')
  end; { InitValueBagTables }

procedure InitValueBagTables2;
  { pre:  ValueListTbl, ValueListRanks defined }
  { post: ValueBagTbl[_].vlr defined }
  var
    vl: ValueList;
    r: ValueBagRank;
  begin
    for r := EmptyValueBagRank to MaxValueBagRank do begin
      with ValueBagTbl[r] do begin
        ValueListFromValueBag(vl, fvb)
      ; vlr := RankFromValueList(vl)
      ; if not(ValueListTbl[vlr].vbr = r) then Fail('InitRestValueBagTables',
              'ValueListTbl[vlr].vbr = r')
      end { with }
    end { for r }
  end; { InitValueBagTables2 }

procedure InitValueSubBags;
  var
    r: ValueBag5Rank;
    i: integer; { actually ValueSubBagIndex+[one beyond] }
    vb: ValueBag;
  begin
    i := 0
  ; for r := NValueBag4Ranks to MaxValueBagRank do
      with ValueBagTbl[r], ValueSubBagIndices[r] do begin
        FirstValueSubBag(vb)
      ; firstvsbi := i
      ; repeat
          ValueSubBags[i].sr := RankFromValueBag(vb)
        ; i := succ(i)
        until not NextValueSubBag(vb, fvb)
      ; lastvsbi := pred(i)
      end { with }
  ; if not(i=4368) then Fail('InitValueSubBags', 'i=4368')
  end; { InitValueSubBags }

procedure InitValueBagCompletions;
  var
    r, rr: ValueBagRank;
    i: integer; { actually ValueBagCompletionIndex+[one beyond] }
    vb: ValueBag;
  begin
    i := 0
  ; for r := 0 to MaxValueBagRank do begin
      with ValueBagTbl[r]{: vbl}, ValueBagRanks[NDice-{vbl.}sz]{: vbr},
           ValueBagCompletionIndices[r]{: vbci} do begin
        {vbci.}firstvbci := i
      ; for rr := {vbr.}firstvbr to {vbr.}lastvbr do begin
          vb := {vbl.}fvb
        ; with ValueBagTbl[rr]{: vbl2}, ValueBagCompletions[i]{: vbc} do begin
            AddValueBag(vb, vb{=vbl.fvb}, {vbl2.}fvb)
          ; {vbc.}cpr := {vbl2.}pr
          ; {vbc.}sum := RankFromValueBag(vb)
          ; i := succ(i)
          end { with }
        end { for rr }
      ; {vbci.}lastvbci := pred(i)
      end { with }
    end { for r }
  ; if not(i=4368) then Fail('InitValueBagCompletions', 'i=4368')
  end; { InitValueBagCompletions }

begin

{AwaitEnter('InitDiceStuff');}
{$IFDEF TEST}
  writeln ( LogFile, 'Initializing DiceStuff ...' )
; Flush ( LogFile )
{$ENDIF}
; MakeEmptyValueList(EmptyValueList)
; MakeEmptyValueBag(EmptyValueBag)
  { Note the order of the next three initializations, see their pre's }
; InitValueBagTables
; InitValueListTables
; InitValueBagTables2
; InitValueSubBags
; InitValueBagCompletions
{$IFDEF TEST}
; writeln ( LogFile, 'DiceStuff initialized' )
; Flush ( LogFile )
{$ENDIF}

end. { unit DiceStuff }
