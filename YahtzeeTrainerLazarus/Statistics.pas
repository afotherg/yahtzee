unit      Statistics;
  { Statistics gathered: frequency counts for rolled dice values and scores.
    Output: mean and median value, standard deviation,
      histogram of distribution, also cumulative (relative)
      percentage above a certain threshold...
}

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
  GeneralStuff,
  DiceStuff, { only for MinValue, MaxValue, DieValue }
  YahtzeeConcepts; { only for MaxScore }

const
  MaxDistributionValue = MaxScore;

type
  DistributionValue = 0..MaxDistributionValue;
  DistributionFrequency = 0..maxlongint;
  Distribution = record
    name: stdstring;
    lower, upper: DistributionValue; { updates must be in [lower..upper] }
    freq: array [ DistributionValue ] of DistributionFrequency;
    minimum, maximum, range, support, median, modeL, modeH: DistributionValue;
    modemult: integer;
    maxfreq, total_actual, total, sum: DistributionFrequency;
    mean, sum2, stddev: real;   
    end; { record }
    { For d: Distribution, freq[lower..upper] is defined;
      total_actual = (+ i:: freq[i]) 
        N.B. Totaling in UpdateDisbribution is generally (though not always)
          less efficient than delaying it until the total is needed.
          However, we use total_actual and total to detect changes and thus
          to avoid unnecessary recomputation of characteristics.
      
      total_act=total implies (ranges for i: lower <= i <= upper)
        minimum = (min i: freq[i]<>0: i), if total<>0
        maximum = (max i: freq[i]<>0: i), if total<>0
        range = maximum - minimum + 1, if total<>0
        support = (# i:: freq[i]<>0)
        median = (max i: (+ j: j<i: freq[j]) <= (+ j: i<=j: freq[j]): i)
               = (max i: 2*(+ j: j<i: freq[j]) <= total: i), if total<>0
        maxfreq = (max i:: freq[i])
        modeL = (min i: freq[i] = maxfreq)
        modeH = (max i: freq[i] = maxfreq)
        modemult = (# i: freq[i] = maxfreq)
        total = (+ i:: freq[i])
        sum   = (+ i:: freq[i]*i)
        mean = sum/total, if total<>0
        sum2 = (+ i:: freq[i]*(i-mean)^2, if total<>0
        stddev = sqrt(sum2/(total-1)), if total>1
      N.B. Special situation for median:
           existence of i in [medianlo..medianhi] with freq[i]=0
        Four medians to choose from:
          M1 = (min i: (+ j: j<i: freq[j]) >= (+ j: i<=j: freq[j]): i)
          M2 = (max i: (+ j: j<i: freq[j]) <= (+ j: i<=j: freq[j]): i)
          M3 = (min i: (+ j: j<=i: freq[j]) >= (+ j: i<j: freq[j]): i)
          M4 = (min i: (+ j: j<=i: freq[j]) >= (+ j: i<j: freq[j]): i)
        I am interested in showing the highest score s such that at least 50%
        of the games end with a score at least s.  This corresponds to M2.
    }
  DetailLevel = (DetailNoZeroes, DetailNoExtremeZeroes, DetailAll);

procedure MakeEmptyDistribution (var {out} d: Distribution;
                                 const nm: stdstring;
                                 lo, up: DistributionValue);
procedure CharacterizeDistribution (var d: Distribution);
procedure WriteDistribution (var f: text; var d: Distribution;
                             n: integer; dl: DetailLevel);
procedure WriteDistributionSummaryHead (var f: text);
procedure WriteDistributionSummaryLine (var f: text; var d: Distribution);
procedure WriteDistributionCharacteristics (var f: text; var d: Distribution);
procedure UpdateDistribution (var d: Distribution;
                              v: integer {DistributionValue};
                              count: DistributionFrequency);

type
  SerialTest = record
    freq: array [ DieValue, DieValue ] of longint;
    prev: DieValue;
    prevvalid: boolean;
  end; { record }

procedure InitSerialTest (var {out} ST: SerialTest);
procedure UpdateSerialTest (var ST: SerialTest; v: DieValue);
function  ChiSqrThreshold (d: integer; p: integer): real;
procedure WriteSerialTest (var f: text; {var} const ST: SerialTest;
                           verbose: boolean);

implementation

procedure MakeEmptyDistribution (var {out} d: Distribution;
                                 const nm: stdstring;
                                 lo, up: DistributionValue);
  { pre:  lo <= up }
  var i: DistributionValue;
  begin
//;writeln('MakeEmptyDistribution: @d="', integer(@d))
//;writeln('MakeEmptyDistribution: @d.name="', integer(@d.name))
//;writeln('MakeEmptyDistribution: nm="', nm, '"')
//;
    if not(lo <= up) then Fail('MakeEmptyDistribution', 'lo <= up')
  ; with d do begin
      name := nm
    ; lower := lo
    ; upper := up
    ; for i := lower to upper do
        freq[i] := 0
    ; total_actual := 0
    end { with d }
//;writeln('MakeEmptyDistribution: d.name="', d.name, '"')
  end; { MakeEmptyDistribution }

procedure CharacterizeDistribution (var d: Distribution);
  { pre:  true
    post: all derived values are up to date }
  var
    i: DistributionValue;
    cum: DistributionFrequency;
  begin
    with d do begin
      if total_actual <> total then begin { outdated }
      ; total := 0 { total = (+ j: lower<=j<i: freq[j]) }
      ; sum := 0   { sum   = (+ j: lower<=j<i: freq[j]*j) }
      ; support := 0 { support = (# j: lower<=j<i: freq[j]<>0) }
      ; maxfreq := 0 { maxfreq = (max j: lower<=j<i: freq[j]) }
      ; modeL := lower { modeL = (min j: lower<=j<i: freq[j]=maxfreq) }
      ; modeH := lower { modeH = (max j: lower<=j<i: freq[j]=maxfreq) }
      ; modemult := 0 { modemult = (# j: lower<=j<i: freq[j]=maxfreq) }
{ maxfreq, modeL, modeH, modemult initialization not according to book }
      ; for i := lower to upper do begin
          total := total + freq[i]
        ; sum := sum + freq[i]*i
        ; support := support + ord(freq[i]<>0)
        ; if freq[i] > maxfreq then begin
            maxfreq := freq[i]
          ; modeL := i
          ; modeH := i
          ; modemult := 1
          end { then }
          else if freq[i] = maxfreq then begin
            modemult := succ(modemult)
          ; modeH := i
          end { if }
        end { for i }
      ; if total <> 0 then begin
          mean := sum / total
        ; minimum := MaxDistributionValue
        ; cum := 0 ; { cum = (+ j: j < i: freq[j]) }
        ; sum2 := 0.0 { sum2 = (+ j: j<i: freq[j]*(j-mean)^2) }
        ; for i := lower to upper do begin
            if 2*cum <= total then
              median := i
          ; cum := cum + freq[i]
          ; if freq[i]<>0 then begin
              if i<minimum then minimum := i
            ; maximum := i
            end { if }
          ; sum2 := sum2 + freq[i]*sqr(i-mean)
          end { for i }
        ; range := maximum - minimum + 1
        ; if total > 1 then
            stddev := sqrt(sum2/(total-1))
        end { if }
      ; if not(total_actual=total) then Fail('CharacterizeDistribution',
              'total_actual=total')
      end { if outdated }
    end { with d }
  end; { CharacterizeDistribution }

procedure WriteDistribution (var f: text; var d: Distribution;
                             n: integer; dl: DetailLevel);
  { pre:  n~ >= 1
    post: representation of d~ written to f~ with groupsize n~ and details dl~ }
  { side-effect: d characterized }
  const
    BarChar = '#';
  var
    i, { iterates over values }
    j: DistributionValue; { first value in group }
    groupfreq,  { total freq in group so far }
    cum,  { cumulative frequency over all preceding values }
    m, { max group freq over all groups }
    h: DistributionFrequency; { cum.freq. over all preceding groups }
    cumpct: real;
    k: integer; { to plot rel.freq. bars }
  begin
    writeln(f)
  ; CharacterizeDistribution(d)
  ; with d do begin
    ; write(f, name, ' Distribution Histogram from ', lower:1, ' to ', upper:1)
    ; if total = 0 then
        writeln(f, ': Empty')
      else begin
        if n <> 1 then
          writeln(f, ' (group size = ', n:1, ')')
        else
          writeln(f)
      ; write(f, 'Value')
      ; if n <> 1 then
          write(f,    ' range')
      ; writeln(f,          '    Frequency    Cum.freq.      Pct  Cum.pct')
      ; write(f, '-----')
      ; if n <> 1 then
          write(f,      '------')
      ; writeln(f,            '  -----------  -----------  -------  -------')
      ; cum := 0 ; { cum = (+ j: j < i: freq[j]) }
      ; m := 0
      ; for i := lower to upper do begin
          if (i mod n = 0) or (i = lower) then { i is first in group }
            groupfreq := 0
        ; groupfreq := groupfreq + freq[i]
        ; if (i mod n = n-1) or (i = upper) then { i is last of group }
            if groupfreq > m then
              m := groupfreq
        end { for i }
        { m determined, m <> 0 because total <> 0 }
      ; for i := lower to upper do begin
          if (i mod n = 0) or (i = lower) then begin { i is first in group }
            j := i
          ; h := cum
          ; groupfreq := 0
          end { if }
        ; groupfreq := groupfreq + freq[i]
        ; if (i mod n = n-1) or (i = upper) then begin { i is last of group }
            cum := cum + groupfreq
          ; cumpct := 100*(cum/total)
          ; if (dl > DetailNoZeroes) or (groupfreq<>0) then
              if (dl > DetailNoExtremeZeroes) or
                 ((0 < cum) and (h < total)) then begin
                write(f, j:5)
              ; if n <> 1 then
                  write(f, i:6)
              ; write(f, '  ', groupfreq:11, '  ', cum:11, '  ',
                      100*(groupfreq/total):6:2, '%  ', cumpct:6:2, '%  ')
              ; for k := 1 to round(20*groupfreq/m) do
                  write(f, BarChar)
              ; writeln(f)
              end { if }
          end { if }
        end { for i }
      end { else }
    end { with d }
  ; flush(f)
  end; { WriteDistribtution }

procedure WriteDistributionSummaryHead (var f: text);
  begin
  ; writeln(f)
  ; writeln(f,
'Name':20,          '  NonZ  Rnge   Min   Max  Medn    Mean  Std.dv   %Lwr')
  ; writeln(f,
'--------------------  ----  ----  ----  ----  ----  ------  ------  -----')
  end; { WriteDistributionSummaryHead }

procedure WriteDistributionSummaryLine (var f: text; var d: Distribution);
  { side-effect: d characterized }
  const
    gw = 2; { gutter width (between columns }
    iw = 4; { # digits shown of integral part }
    fw = 2; { # digits shown of fractional part }
  begin
    CharacterizeDistribution(d)
  ; with d do begin
      write(f, name:20, support:gw+iw)
    ; if total = 0 then
        write(f, '  (Empty)')
      else begin
      ; write(f, range:gw+iw, minimum:gw+iw, maximum:gw+iw,
                 median:gw+iw, mean:gw-1+iw+1+fw:fw)
      ; if total > 1 then
          write(f, stddev:gw-1+iw+1+fw:fw)
        else
          write(f, 'N.A. ':gw-1+iw+1+fw)
      ; write(f, 100*(freq[lower]/total):gw-2+iw+1+fw:fw)
      end { else }
    ; writeln(f)
    end { with d }
  ; flush(f)
  end; { WriteDistributionSummaryLine }

procedure WriteDistributionCharacteristics (var f: text; var d: Distribution);
  { side-effect: d characterized }
  const
    iw = 11; { # digits shown of integral part }
    fw =  2; { # digits shown of fractional part }
  begin
    CharacterizeDistribution(d)
  ; writeln(f)
  ; with d do begin
      write(f, name, ' Distribution Characteristics:')
    ; if total = 0 then
        writeln(f, ' Empty')
      else begin
        writeln(f)
      ; writeln(f, 'Total   = ', total:iw)
      ; writeln(f, 'Sum     = ', sum:iw)
      ; writeln(f, 'Support = ', support:iw)
      ; writeln(f, 'Range   = ', range:iw)
      ; writeln(f, 'Minimum = ', minimum:iw)
      ; writeln(f, 'Maximum = ', maximum:iw)
      ; write  (f, 'Mode    = ', modeL:iw)
      ; if modemult > 1 then 
          write(f, ' (min) to ', modeH:1, ' (max), ',
                   modemult:1, ' occ.')
      ; writeln(f, ' (freq= ', maxfreq:1,
                   ' or ', 100*(maxfreq/total):1:fw, '%)')
      ; writeln(f, 'Median  = ', median:iw)
      ; writeln(f, 'Mean    = ', mean:iw+1+fw:fw)
      ; if total > 1 then
          writeln(f, 'Std.dev.= ', stddev:iw+1+fw:fw)
      end { else }
    end { with d }
  ; flush(f)
  end; { WriteDistributionCharacteristics }

procedure UpdateDistribution (var d: Distribution;
                              v: integer {DistributionValue};
                              count: DistributionFrequency);
  begin
    with d do begin
      if (lower <= v) and (v <= upper) then begin
        freq[v] := freq[v] + count
      ; total_actual := total_actual + count
      end { if }
    end { with d }
  end; { UpdateDistribution }

procedure InitSerialTest (var {out} ST: SerialTest);
  var i, j: DieValue;
  begin
    with ST do begin
      for i := MinValue to MaxValue do
        for j := MinValue to MaxValue do
          freq[i, j] := 0
    ; prevvalid := false
    end { with ST }
  end; { InitSerialTest }

procedure UpdateSerialTest (var ST: SerialTest; v: DieValue);
  begin
    with ST do begin
      if prevvalid then begin
        freq[prev, v] := succ(freq[prev, v])
      ; prevvalid := false
      end { then }
      else begin
        prev := v
      ; prevvalid := true
      end { else }
    end { with ST }
  end; { UpdateSerialTest }

function  ChiSqrThreshold (d: integer; p: integer): real;
  { pre: d >= 30 /\ p in [1, 5, 25, 50, 75, 95, 99]
    ret: k with Pr(Chi^2(d) <= k) = p }
  var xp: real;
  begin
    case p of
       1: xp := -2.33
    ;  5: xp := -1.64
    ; 25: xp := -0.675
    ; 50: xp :=  0.0
    ; 75: xp :=  0.675
    ; 95: xp :=  1.64
    ; 99: xp :=  2.33
    end { case }
 ;  ChiSqrThreshold := d + sqrt(2*d)*xp + 2*sqr(xp)/3 - 2/3
  end; { ChiSqrThreshold }

procedure WriteSerialTest (var f: text; {var} const ST: SerialTest;
                           verbose: boolean);
  const w=8;
  var
    i, j: DieValue;
    n: longint;
    e, v: real;
    d: integer;
  begin
    writeln(f)
  ; with ST do begin
      writeln(f, 'Serial Test Results')
    ; if verbose then begin
        write(f, 'i,j|')
      ; for j := MinValue to MaxValue do
          write(f, j:w)
      ; writeln(f)
      ; write(f, '---+')
      ; for j := MinValue to MaxValue do
          write(f, '--------')
      ; writeln(f)
      end { if }
    ; n := 0
    ; for i := MinValue to MaxValue do begin
        if verbose then
          write(f, i:1, '  |')
      ; for j := MinValue to MaxValue do begin
          if verbose then
            write(f, freq[i, j]:w)
        ; n := n + freq[i, j]
        end { for j }
      ; if verbose then
          writeln(f)
      end { for i }
    ; if n = 0 then
        writeln(f, 'Too few observations: n = ', n:1)
      else begin
        e := n / sqr(NValues)
      ; v := 0.0
      ; for i := MinValue to MaxValue do
          for j := MinValue to MaxValue do
            v := v + sqr(freq[i, j] - e) / e
      ; d := sqr(NValues)-1
      ; writeln(f, 'Chi_', d:1, '^2  = ', v:6:3)
      ; writeln(f, 'Confidence:  1%    5%   25%   50%   75%   95%   99%')
      ; writeln(f, 'Threshold :',
          ChiSqrThreshold(d,  1):6:2,
          ChiSqrThreshold(d,  5):6:2,
          ChiSqrThreshold(d, 25):6:2,
          ChiSqrThreshold(d, 50):6:2,
          ChiSqrThreshold(d, 75):6:2,
          ChiSqrThreshold(d, 95):6:2,
          ChiSqrThreshold(d, 99):6:2)
      end { if }
    end { with ST }
  ; flush(f)
  end; { WriteSerialTest }

end. { unit Statistics }
