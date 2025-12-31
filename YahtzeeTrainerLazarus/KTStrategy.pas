  procedure KarstenAndTobiasStrategy;
    { By Karsten Sperling and Tobias Thierer (Germany)
      Modified by Tom Verhoeff }

    const
      Verbose = false;

    var
      best   : record
                 cat : Category;
                 val : real;
                 vb  : ValueBag;
               end;
      status : integer;
      numOf  : ValueBag;
  
    function hoch (a, b : real) : real;
    { pre: 0 < a
      ret: a^b }
      begin
        hoch := exp(b*ln(a))
      end;
  
    function check (x: integer) : integer;
    { pre: true
      ret: 0 max (max i: MinValue<=i<=MaxValue /\ numOf[i]=x: i) }
      var i: DieValue;
      begin
        check := 0;
        for i := MinValue to MaxValue do
          if numOf[i] = x then
            check := i
      end;
  
    function isRun (a, b: DieValue): boolean;
    { pre: a<=b
      ret: bag [a..b] is contained in numOf }
      var i: integer {DieValue};
      begin
        isRun := true;
        for i := a to b do
          if numOf[i] = 0 then
            isRun := false;
      end;

    procedure updateBest (r: real; c: Category);
      begin
        if Verbose then
          writeln('updateBest(', r:1:1, ', ', CharFromCategory(c), '): ',
                  StringFromValueBag(numOf, false));
        best.val := r;
        best.cat := c;
        best.vb := numOf;
      end;

    procedure selectValues (val: integer);
    { pre:  true
      post: best.vb = best.vb~ min NDice*[val] }
      var i: DieValue;
      begin
        for i := MinValue to MaxValue do
          if (i <> val) then
            best.vb[i] := 0;
        if Verbose then
          writeln('selectValues(', val:1, '): ',
                  StringFromValueBag(best.vb, false))
      end;

    procedure untagFst (val: integer);
      begin
        if val<>0 then
          if best.vb[val] > 0 then
            best.vb[val] := pred(best.vb[val]);
        if Verbose then
          writeln('untagFst(', val:1, '): ',
                  StringFromValueBag(best.vb, false))
      end;
  
    procedure evaluate;
    var i, j, n  : integer;
        ratio    : real;
        catCnt   : Category;
  
    begin { procedure evaluate }
      best.val := -1; best.cat := Aces;
  
      {1er - 6er}
      for catCnt := Aces to Sixes do
        if (catCnt in GS.free) then begin
          ratio := ValueFromCategory(catCnt) * (88/63) *
            (numOf[ValueFromCategory(catCnt)] +
             (5-numOf[ValueFromCategory(catCnt)]) * ((1/6) + (status-1)*(5/36)));
{ Tom: Why 88 and not 63+35=98?  UpperSectionBonus=35
  Can you motivate the formula above?  I do not understand it.
  This also does not take into account
  how much is still needed for U.S. Bonus (GS.usneed) }
          if ratio > best.val then begin
            updateBest(ratio, catCnt);
            selectValues(ValueFromCategory(catCnt));
          end;
        end;
  
      {3er-, 4er-Pasch}
      for catCnt := ThreeOfAKind to FourOfAKind do
        if (catCnt in GS.free) then begin
          if catCnt=ThreeOfAKind then i := 3 else i := 4;
          for j := MinValue to MaxValue do begin
            n := numOf[j];
            if n > i then n := i;
{            if n > 0 then ratio := total else ratio := 0;}
{ Tom: total??? this is total score so far, not total of dice values! }
            ratio := j*n;
            if ratio > best.val then begin
              updateBest(ratio, catCnt);
              selectValues(j);
            end;
          end;
        end;
  
      {Strassen}
      if (SmallStraight in GS.free) and
         (isRun(1,4) or isRun(2,5) or isRun(3,6)) then
        if (30 > best.val) then begin
          updateBest(30, SmallStraight);
          untagFst(check(2)); { do not keep a die that occurs twice }
          if best.vb[2] = 0 then untagFst(1);
          if best.vb[5] = 0 then untagFst(6);
        end;
  
      if (LargeStraight in GS.free) and
         (isRun(1,5) or isRun(2,6)) then
        if (40 > best.val) then begin
          updateBest(40, LargeStraight);
        end;
  
      {Kniffel, i.e. Yahtzee}
      begin
        i := MinValue;
        for j := succ(MinValue) to MaxValue do
          if numOf[j] > numOf[i] then i := j;
        { i = value occuring most often in roll }
        if GS.chip
         then ratio := 100 * hoch((1/6) + (status-1)*(5/36), 5-numOf[i])
         else ratio := 50 * hoch((1/6) + (status-1)*(5/36), 5-numOf[i]);
  
        if ratio > best.val then begin
          updateBest(ratio, Yahtzee); { Tom: What if not(Yahtzee in GS.free)? }
          selectValues(i);
        end;
      end;
  
      {Full House}
      if (FullHouse in GS.free) and
         (check(2) > 0) and (check(3) > 0) then
        if (25 > best.val) then begin
          updateBest(25, FullHouse);
        end;
  
      {Chance}
      if (Chance in GS.free) then begin
        ratio := 0;
        j := status + 3;
        for i := MinValue to j-1 do
          ratio := ratio + i*numOf[i];
        for i := j to MaxValue do
          ratio := ratio + (2.75 + 0.75*status)*numOf[i];
        ratio := ratio / 2;
{ Tom: again, I do not understand the computation above }
        if ratio > best.val then begin
          updateBest(ratio, Chance);
          for i := MinValue to j-1 do
            best.vb[i] := 0;
        end;
      end;
    end; { procedure evaluate }
  
    function chooseBest: Category;
      var
        v : integer;
        catCnt : Category;
      begin
        best.val := -1;
        for catCnt := Aces to Chance do
          if (catCnt in GS.free) then begin
            v := CategoryScores[TS.tsroll].sc[catCnt];
            if catCnt = Chance then v := v div 2;
            if v >= best.val then begin
              updateBest(v, catCnt);
            end;
          end;
        chooseBest := best.cat;
      end; { chooseBest }
  
    begin { procedure KarstenAndTobiasStrategy }
      with YS^, TS, ValueBagTbl[tsroll], ev do begin
        status := succ(rollsleft);
        numOf := fvb;
if Verbose then
  writeln('KarstenAndTobiasStrategy: ', status:1, ', ',
          StringFromValueBag(numOf, false));
        if rollsleft = 0 then begin
          kind := Scoring;
          evcat := chooseBest
        end
        else begin
          evaluate;
          kind := Keeping;
          evkeepers := RankFromValueBag(best.vb);
        end
      end { with }
    end; { procedure KarstenAndTobiasStrategy }

