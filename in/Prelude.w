let true = \t f. t;
    false = \t f. f;

    zero = \s z. z;
    succ = \x s z. s (x s z);
    plus = \x. x succ;
    times = \x. \y. x (plus y) zero;
    isZero = \n. n (\x. false) true;

    one = succ zero;
    two = succ one;
    three = plus one two;
    four = plus two two;

    pair = \x y z. z x y;
    fst  = \p. p (\x y. x);
    snd  = \p. p (\x y. y);

    nil  = \c n. n;
    cons = \h t c n. c h (t c n);
    head = \l. l (\h t. h) false;
    tail = \l c n. l (\h t g. g h (t c)) (\t. n) (\h t. t)
in cons two (cons one nil)
